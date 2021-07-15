module NFA where

import Control.Monad.State
import DFA (DFA, DFAState)
import qualified DFA as D
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Regex
import Prelude hiding (lookup)

data NFAState s a b = NFAState
  { stateId :: Int,
    transition :: a -> Set Int,
    epsilons :: Set Int,
    construct :: Maybe ([a] -> b),
    payload :: Maybe s
  }

instance Show (NFAState s a b) where
  show (NFAState num _ eps _ _) = "NFAState (" ++ show num ++ "," ++ show eps ++ ")"

data NFA s a b
  = NFA
      { nfaStates :: Map Int (NFAState s a b),
        startId :: Int,
        endIds :: Set Int
      }
  | TH
      { nfaStates :: Map Int (NFAState s a b),
        startId :: Int,
        endId :: Int
      }

type Id = Int

type NFAM s a b = State Id (NFA s a b)

type DFAM s a b = State Id (DFA s a b)

newId :: State Id Id
newId = do
  x <- get
  put (x + 1)
  return x

lookup :: NFA s a b -> Int -> NFAState s a b
lookup nfa s = nfaStates nfa M.! s

update :: NFA s a b -> Int -> (NFAState s a b -> NFAState s a b) -> NFA s a b
update nfa s f = nfa {nfaStates = M.insert s (f (lookup nfa s)) (nfaStates nfa)}

closure :: NFA s a b -> Int -> Set Int
closure nfa s = closure' $ S.singleton s
  where
    closure' ss =
      let ss' = S.unions (S.map (epsilons . lookup nfa) ss) `S.union` ss
       in if ss == ss' then ss else closure' ss'

reachable :: NFA s a b -> Int -> a -> Set Int
reachable nfa s a =
  let ss = S.unions $ S.map (\s' -> transition (lookup nfa s') a) $ closure nfa s
   in S.unions $ S.map (closure nfa) ss

dfaEdge :: NFA s a b -> Set Int -> a -> Set Int
dfaEdge nfa ss a = S.unions $ S.map (\s -> reachable nfa s a) ss

matches :: NFA s a b -> [a] -> Bool
matches nfa as = matches' [startId nfa] as
  where
    matches' ss [] = any (`elem` (endIds nfa)) ss
    matches' ss (a : as) = or $ do
      s <- ss
      let ss' = S.toList $ reachable nfa s a
      [matches' ss' as]

empty :: Maybe ([a] -> b) -> NFAM s a b
empty f = do
  do
    x0 <- newId
    x1 <- newId
    let s0 =
          NFAState
            { stateId = x0,
              transition = const S.empty,
              epsilons = S.singleton x1,
              construct = Nothing,
              payload = Nothing
            }
        s1 =
          NFAState
            { stateId = x1,
              transition = const S.empty,
              epsilons = S.empty,
              construct = f,
              payload = Nothing
            }
    return $
      TH
        { nfaStates = M.fromList [(x0, s0), (x1, s1)],
          startId = x0,
          endId = x1
        }

singleton :: Eq a => Maybe ([a] -> b) -> a -> NFAM s a b
singleton f a =
  do
    x0 <- newId
    x1 <- newId
    let s0 =
          NFAState
            { stateId = x0,
              transition = (\a' -> if a == a' then S.singleton x1 else S.empty),
              epsilons = S.empty,
              construct = Nothing,
              payload = Nothing
            }
        s1 =
          NFAState
            { stateId = x1,
              transition = const S.empty,
              epsilons = S.empty,
              construct = f,
              payload = Nothing
            }
    return $
      TH
        { nfaStates = M.fromList [(x0, s0), (x1, s1)],
          startId = x0,
          endId = x1
        }

fromRegex :: Eq a => Maybe ([a] -> b) -> Regex a -> NFA s a b
fromRegex f r =
  let th = evalState (fromRegex' f r) 0
   in NFA
        { nfaStates = nfaStates th,
          startId = startId th,
          endIds = S.singleton $ endId th
        }
  where
    fromRegex' f (Sym a) = singleton f a
    fromRegex' f Empty = empty f
    fromRegex' f (r1 ::: r2) = do
      th_r1 <- fromRegex' Nothing r1
      th_r2 <- fromRegex' f r2
      let th_r1' = update th_r1 (endId th_r1) (\s -> s {epsilons = S.singleton $ startId th_r2})
      return $
        TH
          { nfaStates = nfaStates th_r1 `M.union` nfaStates th_r2,
            startId = startId th_r1,
            endId = endId th_r2
          }
    fromRegex' f (r1 :|| r2) = do
      th_r1 <- fromRegex' Nothing r1
      th_r2 <- fromRegex' Nothing r2
      s_id <- newId
      eId <- newId
      let start =
            NFAState
              { stateId = s_id,
                transition = const S.empty,
                epsilons = S.fromList [startId th_r1, startId th_r2],
                construct = Nothing,
                payload = Nothing
              }
          end =
            NFAState
              { stateId = eId,
                transition = const S.empty,
                epsilons = S.empty,
                construct = f,
                payload = Nothing
              }
          th_r1' = update th_r1 (endId th_r1) (\s -> s {epsilons = S.singleton eId})
          th_r2' = update th_r2 (endId th_r2) (\s -> s {epsilons = S.singleton eId})
      return $
        TH
          { nfaStates = nfaStates th_r1' `M.union` nfaStates th_r2' `M.union` M.fromList [(s_id, start), (eId, end)],
            startId = s_id,
            endId = eId
          }
    fromRegex' f (Star r) = do
      th <- fromRegex' Nothing r
      s_id <- newId
      let start =
            NFAState
              { stateId = s_id,
                transition = const S.empty,
                epsilons = S.singleton $ startId th,
                construct = f,
                payload = Nothing
              }
          th' = update th (endId th) (\s -> s {epsilons = S.singleton $ s_id})
      return $
        TH
          { nfaStates = M.insert s_id start (nfaStates th'),
            startId = s_id,
            endId = s_id
          }

toDFA :: forall s a b. (Show a, Eq a) => NFA s a b -> Set a -> DFA (Set Int) a b
toDFA nfa sigma = evalState toDFA' 0
  where
    toDFA' = do
      s_id <- newId
      let ss = closure nfa (startId nfa)
          start =
            D.DFAState
              { D.stateId = s_id,
                D.transition = error "transition undefined",
                D.construct = Nothing,
                D.payload = ss
              }
          dfa =
            D.DFA
              { D.dfaStates = M.singleton s_id start,
                D.startId = s_id,
                D.endIds = S.empty
              }
      loop dfa $ S.singleton s_id
    loop :: DFA (Set Int) a b -> Set Int -> DFAM (Set Int) a b
    loop dfa todo = do
      (dfa', todo') <- loopOverStates dfa todo
      if null todo' then return dfa' else loop dfa' todo'

    loopOverStates :: DFA (Set Int) a b -> Set Int -> State Id (DFA (Set Int) a b, Set Int)
    loopOverStates dfa todo = foldr loopOverSigma (return (dfa, S.empty)) todo

    loopOverSigma :: Int -> State Id (DFA (Set Int) a b, Set Int) -> State Id (DFA (Set Int) a b, Set Int)
    loopOverSigma k dfaM =
      (\b as f -> S.foldr' f b as) dfaM sigma $ \a dfaM' -> do
        (dfa', todo) <- dfaM'
        let next = dfaEdge nfa (D.lookupPayload dfa' k) a
            isEnd = any (`elem` endIds nfa) $ S.toList next
        case D.lookupId dfa' next of
          Just s ->
            let dfa'' = D.updateTrans dfa' k (\t -> (\x -> if x == a then s else t x))
                dfa''' = if isEnd then dfa'' {D.endIds = S.insert s $ D.endIds dfa''} else dfa''
             in return (dfa''', todo)
          Nothing -> do
            s_new <- newId
            let new_state =
                  D.DFAState
                    { D.stateId = s_new,
                      D.transition = undefined,
                      D.construct = Nothing,
                      D.payload = next
                    }
                dfa'' = D.updateTrans dfa' k (\t -> (\x -> if x == a then s_new else t x))
                dfa''' = if isEnd then dfa'' {D.endIds = S.insert s_new $ D.endIds dfa''} else dfa''
            return (D.insert dfa''' s_new new_state, S.insert s_new todo)
