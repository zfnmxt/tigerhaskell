module Lexer.FA where

import Control.Monad
import Data.Bifunctor
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Types

mapNodes :: (Functor m, Ord s', Ord a) => (s -> s') -> FA m a s -> FA m a s'
mapNodes f fa =
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
                  if S.null qs
                    then []
                    else [((a, ss), qs)]
              new_states = S.map snd new_transitions
           in construct
                (transitions `S.union` new_transitions)
                (seen `S.union` new_states)
                (new_states S.\\ todo)

simplifyStates :: (Functor m, Enum s, Ord a, Ord s) => FA m a s -> FA m a s
simplifyStates fa = mapNodes (toEnum . (m M.!)) fa
  where
    m = M.fromList $ zip (S.elems $ states fa) [1 ..]

-- toDFA_ :: (Enum s, Ord a, Ord s) => NFA a s -> DFA a s
-- toDFA_ nfa = mapNodes (toEnum . (m M.!)) dfa
--  where
--    dfa = toDFA nfa
--    m = M.fromList $ zip (S.elems $ states dfa) [1 ..]

unions :: (Ord s, Ord a, Node s) => [NFA a s] -> NFA a s
unions = foldr1 union

union :: (Ord s, Ord a, Node s) => NFA a s -> NFA a s -> NFA a s
union nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s,
      delta_e = link <> delta_e nfa1_s <> delta_e nfa2_s,
      start = startNode,
      accept = accept nfa1_s `S.union` accept nfa2_s,
      states = S.insert startNode $ states nfa1_s `S.union` states nfa2_s,
      alphabet = alphabet nfa1 `S.union` alphabet nfa2
    }
  where
    [nfa1_s, nfa2_s] = renameNodes [nfa1, nfa2]
    link = F.fromList [(startNode, start nfa1_s), (startNode, start nfa2_s)]

concat :: (Ord s, Ord a, Node s) => NFA a s -> NFA a s -> NFA a s
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
    [nfa1_s, nfa2_s] = renameNodes [nfa1, nfa2]
    link =
      F.fromList $ map (,start nfa2_s) $ S.toList $ accept nfa1_s

star :: (Ord s, Ord a, Node s) => NFA a s -> NFA a s
star nfa =
  FA
    { delta = delta nfa_s,
      delta_e = F.singleton startNode (start nfa_s) <> loop <> delta_e nfa_s,
      start = startNode,
      accept = S.insert startNode $ accept nfa_s,
      states = S.insert startNode $ states nfa_s,
      alphabet = alphabet nfa_s
    }
  where
    [nfa_s] = renameNodes [nfa]
    loop =
      F.fromList $ map (,start nfa_s) $ S.toList $ accept nfa_s

instance Node String where
  startNode = "start"
  nextNode = (++ "_")
  renameFun _ i = (++ ("_" ++ show i))

instance Node Int where
  startNode = 0
  nextNode = (+ 1)
  renameFun fas i =
    let global_max = maximum $ map (S.findMax . states) fas
     in (+ i * global_max)

renameNodes :: (Functor m, Ord a, Node s, Ord s) => [FA m a s] -> [FA m a s]
renameNodes fas = zipWith (\fa i -> mapNodes (renameFun fas i) fa) fas [1 ..]
