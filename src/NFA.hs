module NFA where

import Control.Monad.State
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Regex
import Prelude hiding (lookup)

data NFAState a b = NFAState
  { stateNum :: Int,
    transition :: a -> [Int],
    epsilons :: [Int],
    construct :: Maybe ([a] -> b)
  }

instance Show (NFAState a b) where
  show (NFAState num _ eps _) = "NFAState (" ++ show num ++ "," ++ show eps ++ ")"

data NFA a b
  = NFA
      { nfaStates :: Map Int (NFAState a b),
        startState :: Int,
        endStates :: [Int]
      }
  | TH
      { nfaStates :: Map Int (NFAState a b),
        startState :: Int,
        endState :: Int
      }

type NFAM a b = State Int (NFA a b)

newNum :: State Int Int
newNum = do
  x <- get
  put (x + 1)
  return x

lookup :: NFA a b -> Int -> NFAState a b
lookup nfa s = nfaStates nfa M.! s

closure :: NFA a b -> Int -> [Int]
closure nfa s = closure' [s]
  where
    closure' ss =
      let ss' = nub (concatMap (epsilons . lookup nfa) ss ++ ss)
       in if ss == ss' then ss else closure' ss'

reachable :: NFA a b -> Int -> a -> [Int]
reachable nfa s a =
  let ss = nub (concatMap (\s' -> transition (lookup nfa s') a) (closure nfa s))
   in nub (concatMap (closure nfa) ss)

matches :: NFA a b -> [a] -> Bool
matches nfa as = matches' [startState nfa] as
  where
    matches' ss [] = any (`elem` (endStates nfa)) ss
    matches' ss (a : as) = or $ do
      s <- ss
      let ss' = reachable nfa s a
      [matches' ss' as]

singleton :: Eq a => ([a] -> b) -> a -> NFAM a b
singleton f a =
  do
    x0 <- newNum
    x1 <- newNum
    let s0 =
          NFAState
            { stateNum = x0,
              transition = (\a' -> if a == a' then [x1] else []),
              epsilons = [],
              construct = Nothing
            }
        s1 =
          NFAState
            { stateNum = x1,
              transition = const [],
              epsilons = [],
              construct = Just f
            }
    return $
      TH
        { nfaStates = M.fromList [(x0, s0), (x1, s1)],
          startState = x0,
          endState = x1
        }

fromRegex :: Eq a => ([a] -> b) -> Regex a -> NFA a b
fromRegex f r =
  let th = evalState (fromRegex' f r) 0
   in NFA
        { nfaStates = nfaStates th,
          startState = startState th,
          endStates = [endState th]
        }
  where
    fromRegex' f (Sym a) = singleton f a
    fromRegex' f (r1 ::: r2) = do
      th_r1 <- fromRegex' (error "Not an end state") r1
      th_r2 <- fromRegex' f r2
      let r1_end = nfaStates th_r1 M.! endState th_r1
          r1_end' = r1_end {epsilons = [startState th_r2]}
          r1_states = M.insert (endState th_r1) r1_end' (nfaStates th_r1)
          r2_states = nfaStates th_r2
      return $
        TH
          { nfaStates = r1_states `M.union` r2_states,
            startState = startState th_r1,
            endState = endState th_r2
          }
