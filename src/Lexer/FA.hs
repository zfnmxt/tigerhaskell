module Lexer.FA where

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Types

lookupPayload :: Ord s => FA m a s p -> s -> p
lookupPayload fa = (payloads fa M.!)

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

withComp :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> ([c] -> m (a, [c])) -> [c] -> s -> m (s, [c])
withComp fa f cs s = do
  (a, cs') <- f cs
  (,cs') <$> step fa a s

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

intWithComp :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> ([c] -> m (a, [c])) -> [c] -> s -> m s
intWithComp fa f cs s = do
  (s', cs') <- withComp fa f cs s
  if null cs'
    then pure s'
    else intWithComp fa f cs' s'

greedyInt :: (Eq (m s), MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> s -> m (s, [a])
greedyInt fa [] s = do
  s' <- stepE fa s
  pure (s', [])
greedyInt fa input@(a : as) s
  | step fa a s == mzero =
      pure (s, input)
  | otherwise = do
      s' <- step fa a s
      greedyInt fa as s'

greedyInt_ :: (Eq (m s), MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> m (s, [a])
greedyInt_ fa as = greedyInt fa as $ start fa

greedyIntDFA_ :: (Ord a, Ord s) => DFA a s p -> [a] -> (s, [a])
greedyIntDFA_ dfa as = fromMaybe (error "") $ greedyInt_ dfa as

int :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> s -> m s
int fa [] s = stepE fa s
int fa as s = foldM (flip $ step fa) s as

int_ :: (MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> m s
int_ fa as = int fa as $ start fa

accepts :: (Foldable m, MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> [a] -> Bool
accepts fa as = or $ (`S.member` accept fa) <$> int_ fa as

closure :: (Ord a, Ord s, Ord p) => NFA a s p -> S.Set s -> S.Set s
closure = F.reachables . delta_e

dfaEdge :: (Ord a, Ord s, Ord p) => NFA a s p -> a -> S.Set s -> S.Set s
dfaEdge nfa a ss = closure nfa $ S.fromList [s' | s <- S.toList ss, s' <- F.int (delta nfa) (a, s)]

toDFA :: (Ord a, Ord s, Ord p, Enum s) => NFA a s p -> DFA a s p
toDFA nfa = mapNodes (toEnum . (m M.!)) dfa
  where
    m = M.fromList $ zip (S.elems $ states dfa) [1 ..]
    dfa =
      FA
        { delta = delta',
          delta_e = F.empty,
          start = start',
          accept = accept',
          states = states',
          alphabet = alphabet nfa,
          payloads = payloads'
        }
    (states', delta') = loop mempty (S.singleton start') F.empty
    start' = F.reachable (delta_e nfa) (start nfa)
    accept' = S.fromList [s | s <- S.toList states', not $ null (s `S.intersection` accept nfa)]
    payloads' =
      M.unions
        $ map
          ( \ss ->
              case mapMaybe (payloads nfa M.!?) (S.toList ss) of
                [] -> mempty
                ps -> M.singleton ss (minimum ps)
          )
        $ S.toList states'
    loop seen todo f
      | S.null todo = (seen, f)
      | otherwise =
          let (ss : rest) = S.toList todo
              (f', todo') = construct f ss seen
           in loop (ss `S.insert` seen) (todo' `S.union` S.fromList rest) f'
    construct f ss seen =
      foldl (fold_fun ss seen) (f, mempty) $ S.toList $ alphabet nfa
    fold_fun ss seen (f, todo) a =
      let e = dfaEdge nfa a ss
          f'
            | S.null e = f
            | otherwise = F.singleton (a, ss) e F.<+ f
          todo'
            | e `S.member` seen = todo
            | otherwise = e `S.insert` todo
       in (f', todo')

unions :: Ord a => [NFA a Int p] -> NFA a Int p
unions = foldl1 union

union :: (Ord s, Ord a) => NFA a s p -> NFA a s p -> NFA a Int p
union nfa1 nfa2 =
  FA
    { delta = delta nfa1_s <> delta nfa2_s,
      delta_e = link <> delta_e nfa1_s <> delta_e nfa2_s,
      start = 0,
      accept = accept nfa1_s <> accept nfa2_s,
      states = S.insert 0 $ states nfa1_s <> states nfa2_s,
      alphabet = alphabet nfa1_s <> alphabet nfa2_s,
      payloads = payloads nfa1_s <> payloads nfa2_s
    }
  where
    [nfa1_s, nfa2_s] = renameFAS [nfa1, nfa2]
    link = F.fromList [(0, start nfa1_s), (0, start nfa2_s)]

concat :: (Ord s, Ord a) => NFA a s p -> NFA a s p -> NFA a Int p
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
    [nfa1_s, nfa2_s] = renameFAS [nfa1, nfa2]
    link =
      F.fromList $ map (,start nfa2_s) $ S.toList $ accept nfa1_s

star :: (Ord s, Ord a) => NFA a s p -> NFA a s p
star nfa =
  nfa
    { delta_e = loop <> delta_e nfa,
      accept = S.insert (start nfa) $ accept nfa
    }
  where
    loop =
      F.fromList $ map (,start nfa) $ S.toList $ accept nfa

renameFA :: (Functor m, Ord a, Ord s) => FA m a s p -> Int -> FA m a Int p
renameFA fa strt = mapNodes (m M.!) fa
  where
    m = M.fromList $ zip (S.toList $ states fa) [strt ..]

renameFAS :: (Functor m, Ord a, Ord s) => [FA m a s p] -> [FA m a Int p]
renameFAS =
  fst
    . foldr
      ( \fa (fas, strt) ->
          let fa' = renameFA fa strt
           in (fa' : fas, maximum (states fa') + 1)
      )
      ([], 1)

oneOf :: Ord a => [a] -> NFA a Int String
oneOf as =
  FA
    { delta = F.fromList [((a, 0), 1) | a <- as],
      delta_e = F.empty,
      start = 0,
      accept = S.singleton 1,
      states = S.fromList [0, 1],
      alphabet = S.fromList as,
      payloads = mempty
    }

plus :: Ord a => NFA a Int String -> NFA a Int String
plus nfa = nfa `Lexer.FA.concat` star nfa
