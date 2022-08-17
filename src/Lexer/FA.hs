{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Lexer.FA where

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import Data.Bifunctor
import Data.Foldable
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Types

data Env c s g = Env
  { envConsumed :: [c],
    envRest :: [c],
    envState :: s,
    envMisc :: g
  }

class MonadDFA m c a s | m -> c a s where
  faLook :: m (Maybe a)
  faNext :: m ()
  faOnStep :: a -> m ()

faUntil ::
  ( Ord a,
    Ord s,
    MonadState (Env c s g) m,
    MonadReader (DFA a s p) m
  ) =>
  MonadDFA m c a s =>
  m b ->
  m b ->
  m b
faUntil accepts rejects = do
  dfa <- ask
  s <- gets envState
  ma <- faLook
  case ma of
    Nothing ->
      if s `S.member` accept dfa
        then accepts
        else rejects
    Just a ->
      case step dfa a s of
        Nothing ->
          if s `S.member` accept dfa
            then accepts
            else rejects
        Just s' -> do
          faNext
          modify $ \env -> env {envState = s'}
          faOnStep a
          faUntil accepts rejects

--  dfa <- ask
--  s <- gets envState
--  ma <- faNext
--  case ma of
--    Nothing ->
--      if s `S.member` accept dfa
--        then accepts
--        else rejects
--    Just a ->
--      case step dfa a s of
--        Nothing ->
--          if s `S.member` accept dfa
--            then accepts
--            else rejects
--        Just s' -> do
--          modify $ \env -> env {envState = s'}
--          onStep a s s'
--          faAccepts faNext onStep accepts rejects

-- class MonadFA m t a s p where
--  faNext :: m (Maybe a)
--  faState :: t s
--  faStep :: (s -> t m ()) -> (s -> t m ()) -> t m Bool
--  faAccepts :: (s -> t m ()) -> (s -> t m ()) -> t m Bool
--
-- instance
--  ( Ord a,
--    Ord s,
--    Env e a s,
--    MonadTrans t,
--    MonadPlus (t m),
--    MonadState e m,
--    MonadReader (FA (t m) a s p) m,
--    MonadError g m
--  ) =>
--  MonadFA m t a s p
--  where
--  faNext = do
--    rest <- gets envRest
--    case rest of
--      [] -> pure Nothing
--      (c : cs) -> do
--        modify $ envUpdRest (const cs) . envUpdConsumed (++ [c])
--        pure $ Just c
--  faStep f_n f_j = do
--    ma <- lift $ faNext
--    s <- lift $ gets envState
--    case ma of
--      Nothing -> do
--        f_n s
--        pure False
--      Just a -> do
--        fa <- lift $ ask
--        s' <- step fa a s
--        lift $ modify $ envUpdState (const s')
--        pure True
--  faAccepts f_n f_j = do
--    success <- faStep f_n f_j
--    if success
--      then faAccepts f_n f_j
--      else do
--        s <- lift $ gets envState
--        fa <- lift $ ask
--        pure $ s `S.member` accept fa

-- data FAEnv a s = FAEnv
--  { envConsumed :: [a],
--    envRest :: [a],
--    envState :: s
--  }
--
-- class MonadFA m t a s p where
--  faNext :: m (Maybe a)
--  faState :: m s
--  faStep :: (s -> t m ()) -> (s -> t m ()) -> t m ()
--
-- instance
--  ( Ord a,
--    Ord s,
--    MonadTrans t,
--    MonadPlus (t m),
--    MonadState (FAEnv a s) m,
--    MonadReader (FA (t m) a s p) m
--  ) =>
--  MonadFA m t a s p
--  where
--  faNext = do
--    rest <- gets envRest
--    case rest of
--      [] -> pure Nothing
--      (c : cs) -> do
--        modify (\env -> env {envRest = cs, envConsumed = envConsumed env ++ [c]})
--        pure $ Just c
--  faState = gets envState
--  faStep f_n f_j = do
--    ma <- lift $ faNext
--    s <- lift $ gets envState
--    case ma of
--      Nothing -> f_n s
--      Just a -> do
--        fa <- lift $ ask
--        s' <- step fa a s
--        lift $ modify (\env -> env {envState = s'})
--        pure ()

-- next :: FAM a s g (Maybe a)
-- next = do
--  rest <- gets envRest
--  case rest of
--    [] -> pure Nothing
--    (c : cs) -> do
--      modify (\env -> env {envRest = cs, envConsumed = envConsumed env ++ [c]})
--      pure $ Just c
--
---- stepM :: (MonadState z, MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> a -> s -> m s
-- stepM :: (MonadPlus m, Monad m, Ord a, Ord s) => (Maybe a -> s -> b -> m c) -> FAM a s g ()
-- stepM step_fun = do
--  a <- next
--  case a of
--    Nothing -> step
--
-- data FAEnv a s g = FAEnv
--  { envConsumed :: [a],
--    envRest :: [a],
--    envState :: s,
--    envMisc :: g
--  }
--
-- type FAM a s g = MonadState (FAEnv a s g)

-- next :: FAM a s g ->
----  faNext = do
----    rest <- gets envRest
----    case rest of
----      [] -> pure Nothing
----      (c : cs) -> do
----        modify (\env -> env {envRest = cs, envConsumed = envConsumed env ++ [c]})
----        -- modify $ envUpdRest (const cs) . envUpdConsumed (++ [c])
----        pure $ Just c
--
----class MonadFA a s m where
----  faNext :: m (Maybe a)
----  faState :: m s
----  faStep :: m b -> (Maybe a -> s -> b -> m c) -> m c
----
----instance (MonadState (FAEnv a s g) m) => MonadFA a s m where
----  faNext = do
----    rest <- gets envRest
----    case rest of
----      [] -> pure Nothing
----      (c : cs) -> do
----        modify (\env -> env {envRest = cs, envConsumed = envConsumed env ++ [c]})
----        -- modify $ envUpdRest (const cs) . envUpdConsumed (++ [c])
----        pure $ Just c
----  faState = gets envState
----  faStep mb f = do
----    b <- mb
----    a <- faNext
----    s <- faState
----    f a s b
--
--
--
----stepM :: (MonadState z, MonadPlus m, Monad m, Ord a, Ord s) => FA m a s p -> a -> s -> m s
-- stepM :: (MonadPlus m, Monad m, Ord a, Ord s) => (Maybe a -> s -> b -> m c) -> FAM a s g ()
-- stepM step_fun = do
--  <- gets envRest
--
--
--  do
--  s_e <- stepE fa s
--  s_e' <- F.int (delta fa) (a, s_e)
--  stepE fa s_e'

-- class FAEnv e where
--  envConsumed :: e -> [a]
--  envUpdConsumed :: ([a] -> [a]) -> e -> e
--  envRest :: e -> [a]
--  envUpdRest :: ([a] -> [a]) -> e -> e
--  envState :: e -> s
--  envUpdState :: (s -> s) -> e -> e
--
-- class MonadFA a s m where
--  next :: m (Maybe a)
--  state :: m s
--
-- instance (FAEnv e, MonadState e m) => MonadFA a s m where
--  next = do
--    rest <- gets envRest
--    case rest of
--      [] -> pure Nothing
--      (c : cs) -> do
--        modify $ envUpdRest (const cs) . envUpdConsumed (++ [c])
--        pure $ Just c
--  state = gets envState

--
-- class Monad m => FAM m k a s p where
--  next :: m (Maybe a)
--  state :: m (k s)
--  doStep :: (k s -> m z) -> a -> m z
--  getFA :: m (FA k a s p)
--  run :: (k s -> m z) -> [a] -> m [z]
--
-- instance (FAEnv e, MonadState e m, MonadPlus k, Ord a, Monad k, Ord s) => FAM m k a s p where
--  next = do
--    rest <- gets envRest
--    case rest of
--      [] -> pure Nothing
--      (c : cs) -> do
--        modify $ envUpdRest (const cs) . envUpdConsumed (++ [c])
--        pure $ Just c
--  state = gets envState
--  getFA = gets envFA
--  doStep f a = do
--    fa <- (getFA :: m (FA k a s p))
--    modify $ envUpdState (step fa a =<<)
--    gets envState >>= f
--  run f = mapM (doStep f)

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
              (f', todo') = construct todo f ss seen
           in loop (ss `S.insert` seen) (todo' `S.union` S.fromList rest) f'
    construct todo f ss seen =
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
renameFA fa start = mapNodes (m M.!) fa
  where
    m = M.fromList $ zip (S.toList $ states fa) [start ..]

renameFAS :: (Functor m, Ord a, Ord s) => [FA m a s p] -> [FA m a Int p]
renameFAS =
  fst
    . foldr
      ( \fa (fas, start) ->
          let fa' = renameFA fa start
           in (fa' : fas, (maximum $ states fa') + 1)
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
