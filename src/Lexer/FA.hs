module Lexer.FA where

import Control.Monad
import Data.Bifunctor
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Types

mapStates :: (Functor m, Ord s', Ord a) => (s -> s') -> FA m a s -> FA m a s'
mapStates f fa =
  fa
    { delta = F.map (second f) f $ delta fa,
      delta_e = F.map f f $ delta_e fa,
      start = f $ start fa,
      accept = S.map f $ accept fa,
      states = S.map f $ states fa
    }

step :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s -> a -> s -> m s
step fa a s = do
  s_e <- stepE fa s
  s_e' <- F.int (delta fa) (a, s_e)
  stepE fa s_e'

stepE :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s -> s -> m s
stepE fa s = mplus (pure s) $ do
  s' <- F.int (delta_e fa) s
  stepE fa s'

int :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s -> [a] -> s -> m s
int fa [] s = stepE fa s
int fa as s = foldM (flip $ step fa) s as

int_ :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s -> [a] -> m s
int_ fa as = int fa as $ start fa

accepts :: (Foldable m, MonadPlus m, Monad m, Ord a, Ord s) => FA m a s -> [a] -> Bool
accepts fa as = or $ (`S.member` accept fa) <$> int_ fa as

toDFA :: (Ord a, Ord s) => NFA a s -> DFA a (S.Set s)
toDFA nfa =
  FA
    { delta = delta',
      delta_e = F.empty,
      start = start',
      accept = accept',
      states = states',
      alphabet = alphabet nfa
    }
  where
    (delta', states') = construct mempty (S.singleton start') (S.singleton start')
    start' = F.reachable (delta_e nfa) (start nfa)
    accept' = S.fromList [s | s <- S.toList states', not $ null (s `S.intersection` accept nfa)]
    construct transitions seen todo
      | S.null todo = (F.fromSet transitions, seen)
      | otherwise =
          let new_transitions =
                S.fromList $ do
                  ss <- S.toList todo
                  a <- S.toList $ alphabet nfa
                  let qs =
                        S.fromList
                          [ q | s <- S.toList ss, q <- S.toList $ states nfa, q `S.member` F.reachables (delta_e nfa) (S.fromList $ F.int (delta nfa) (a, s))
                          ]
                  pure ((a, ss), qs)
              new_states = S.map snd new_transitions
           in construct
                (transitions `S.union` new_transitions)
                (seen `S.union` new_states)
                (new_states S.\\ todo)

-- delta' = F.fromList $ do
--  ss <- S.toList states'
--  a <- S.toList $ alphabet nfa
--  let qs =
--        S.fromList
--          [ q | s <- S.toList ss, q <- S.toList $ states nfa, q `S.member` F.reachables (delta_e nfa) (S.fromList $ F.int (delta nfa) (a, s))
--          ]
--  return ((a, ss), qs)

union :: (Ord a, ToString s) => NFA a s -> NFA a s -> NFA a String
union nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s,
      delta_e = link <> delta_e nfa1_s <> delta_e nfa2_s,
      start = "start",
      accept = accept nfa1_s `S.union` accept nfa2_s,
      states = S.insert "start" $ states nfa1_s `S.union` states nfa2_s,
      alphabet = alphabet nfa1 `S.union` alphabet nfa2
    }
  where
    nfa1_s = mapStates ((++ "_1") . toString) nfa1
    nfa2_s = mapStates ((++ "_2") . toString) nfa2
    link = F.fromList [("start", start nfa1_s), ("start", start nfa2_s)]

concat :: (Ord a, ToString s) => NFA a s -> NFA a s -> NFA a String
concat nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s,
      delta_e = link <> delta_e nfa1_s <> delta_e nfa2_s,
      start = start nfa1_s,
      accept = accept nfa2_s,
      states = states nfa1_s `S.union` states nfa2_s,
      alphabet = alphabet nfa1 `S.union` alphabet nfa2
    }
  where
    nfa1_s = mapStates ((++ "_1") . toString) nfa1
    nfa2_s = mapStates ((++ "_2") . toString) nfa2
    link =
      F.fromList $ map (,start nfa2_s) $ S.toList $ accept nfa1_s

star :: (Ord a, ToString s) => NFA a s -> NFA a String
star nfa =
  FA
    { delta = delta nfa_s,
      delta_e = F.singleton "start" (start nfa_s) <> loop <> delta_e nfa_s,
      start = "start",
      accept = S.insert "start" $ accept nfa_s,
      states = S.insert "start" $ states nfa_s,
      alphabet = alphabet nfa_s
    }
  where
    nfa_s = mapStates ((++ "_") . toString) nfa
    loop =
      F.fromList $ map (,start nfa_s) $ S.toList $ accept nfa_s

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance Show a => ToString a where
  toString = show

-- dfaReachable :: (Ord a, Ord s) => DFA a s -> S.Set s
-- dfaReachable dfa = explore (S.singleton $ start dfa) (S.singleton $ start dfa)
--  where
--    explore seen todo
--      | S.null todo = seen
--      | otherwise =
--          let new_states =
--                S.fromList $ do
--                  s <- S.toList todo
--                  a <- S.toList $ alphabet dfa
--                  case F.int (delta dfa) (a, s) of
--                    Nothing -> []
--                    Just s' -> [s']
--           in explore (seen `S.union` new_states) (new_states S.\\ todo)
--
-- trimDFA :: (Ord a, Ord s) => DFA a s -> DFA a s
-- trimDFA dfa = dfa {delta = delta', states = reachable}
--  where
--    delta' = F.filter f $ delta dfa
--    reachable = dfaReachable dfa
--    f (a, s) (Just s') =
--      s `S.member` reachable && s' `S.member` reachable
--    f _ _ = False
