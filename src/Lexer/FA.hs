{-# LANGUAGE ConstrainedClassMethods #-}

module Lexer.FA where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Types

mapNodes :: (Functor m, Ord s', Ord a) => (s -> s') -> FA m a s p -> FA m a s' p
mapNodes f fa =
  fa
    { delta = F.map (second f) f $ delta fa,
      delta_e = F.map f f $ delta_e fa,
      start = f $ start fa,
      accept = S.map f $ accept fa,
      states = S.map f $ states fa,
      payloads = M.mapKeys f $ payloads fa
    }

step :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> a -> s -> m s
step fa a s = do
  s_e <- stepE fa s
  s_e' <- F.int (delta fa) (a, s_e)
  stepE fa s_e'

stepE :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> s -> m s
stepE fa = stepE' S.empty
  where
    stepE' seen s
      | s `S.member` seen = mzero
      | otherwise =
          mplus (pure s) $ do
            s' <- F.int (delta_e fa) s
            stepE' (S.insert s seen) s'

int :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> s -> m s
int fa [] s = stepE fa s
int fa as s = foldM (flip $ step fa) s as

int_ :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> m s
int_ fa as = int fa as $ start fa

accepts :: (Foldable m, MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> Bool
accepts fa as = or $ (`S.member` accept fa) <$> int_ fa as

toDFA :: (Ord a, Ord s, Ord p) => NFA a s p -> DFA a (S.Set s) (S.Set p)
toDFA nfa =
  FA
    { delta = delta',
      delta_e = F.empty,
      start = start',
      accept = accept',
      states = states',
      alphabet = alphabet nfa,
      payloads = payloads'
    }
  where
    (delta', states') = construct mempty mempty (S.singleton start')
    start' = F.reachable (delta_e nfa) (start nfa)
    accept' = S.fromList [s | s <- S.toList states', not $ null (s `S.intersection` accept nfa)]
    payloads' =
      M.fromList $
        map (\ss -> (ss, S.fromList $ mapMaybe (payloads nfa M.!?) (S.toList ss))) $
          S.toList states'
    construct transitions seen todo
      | S.null todo = (F.fromSet transitions, seen)
      | otherwise =
          let (ss : rest) = S.toList todo
              new_transitions =
                S.fromList $
                  do
                    a <- S.toList $ alphabet nfa
                    let qs =
                          S.fromList
                            [ q | s <- S.toList ss, q <- S.toList $ states nfa, q `S.member` F.reachables (delta_e nfa) (S.fromList $ F.int (delta nfa) (a, s))
                            ]
                    if S.null qs
                      then []
                      else [((a, ss), qs)]
              new_states = S.map snd new_transitions
              seen' = ss `S.insert` seen
              todo' = (S.fromList rest `S.union` new_states) S.\\ seen'
           in construct
                (transitions `S.union` new_transitions)
                seen'
                todo'

simplifyStates :: (Functor m, Enum s, Ord a, Ord s) => FA m a s p -> FA m a s p
simplifyStates fa = mapNodes (toEnum . (m M.!)) fa
  where
    m = M.fromList $ zip (S.elems $ states fa) [1 ..]

toDFAFlat :: (Enum s, Ord a, Ord s, Ord p) => NFA a s p -> DFA a s (S.Set p)
toDFAFlat nfa = mapNodes (toEnum . (m M.!)) dfa
  where
    dfa = toDFA nfa
    m = M.fromList $ zip (S.elems $ states dfa) [1 ..]

unions :: (Ord s, Ord a, Node s) => [NFA a s p] -> NFA a s p
unions = foldr1 union

union :: (Ord s, Ord a, Node s) => NFA a s p -> NFA a s p -> NFA a s p
union nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s',
      delta_e = delta_e nfa1_s <> delta_e nfa2_s',
      start = start nfa1_s,
      accept = accept nfa1_s `S.union` accept nfa2_s',
      states = states nfa1_s `S.union` states nfa2_s',
      alphabet = alphabet nfa1_s `S.union` alphabet nfa2_s',
      payloads = payloads nfa1_s <> payloads nfa2_s'
    }
  where
    [nfa1_s, nfa2_s] = renameNodes [nfa1, nfa2]
    sub x y z
      | z == x = y
      | otherwise = x
    nfa2_s' = mapNodes (sub (start nfa2_s) (start nfa1_s)) nfa2_s

concat :: (Ord s, Ord a, Node s) => NFA a s p -> NFA a s p -> NFA a s p
concat nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s,
      delta_e = link <> delta_e nfa1_s <> delta_e nfa2_s,
      start = start nfa1_s,
      accept = accept nfa2_s,
      states = states nfa1_s `S.union` states nfa2_s,
      alphabet = alphabet nfa1 `S.union` alphabet nfa2,
      payloads = payloads nfa1_s <> payloads nfa2_s
    }
  where
    [nfa1_s, nfa2_s] = renameNodes [nfa1, nfa2]
    link =
      F.fromList $ map (,start nfa2_s) $ S.toList $ accept nfa1_s

star :: (Ord s, Ord a, Node s) => NFA a s p -> NFA a s p
star nfa =
  nfa
    { delta_e = loop <> delta_e nfa,
      accept = S.insert (start nfa) $ accept nfa
    }
  where
    loop =
      F.fromList $ map (,start nfa) $ S.toList $ accept nfa

renameNodes :: (Functor m, Ord a, Node s, Ord s) => [FA m a s p] -> [FA m a s p]
renameNodes fas = zipWith (\fa i -> mapNodes (renameFun fas i) fa) fas [1 ..]

instance Node String where
  startNode = "start"
  nextNode = (++ "_")
  renameFun _ i = (++ ("_" ++ show i))

instance Node Int where
  startNode = 0
  nextNode = (+ 1)
  renameFun fas i =
    let global_max = (maximum $ map (S.findMax . states) fas)
     in (+ i * (global_max + 1))
