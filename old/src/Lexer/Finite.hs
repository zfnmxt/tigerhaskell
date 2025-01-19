module Lexer.Finite where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Prelude

newtype Fin m x y = Fin (Map x (m y)) deriving (Show, Read)

instance Functor m => Functor (Fin m x) where
  fmap f (Fin m) = Fin $ (fmap . fmap) f m

instance (MonadPlus m, Ord x, Ord y) => Semigroup (Fin m x y) where
  Fin f <> Fin g = Fin $ M.unionWith mplus f g

instance (MonadPlus m, Ord x, Ord y) => Monoid (Fin m x y) where
  mempty = Lexer.Finite.empty

empty :: Fin m x y
empty = Fin M.empty

fromMap :: MonadPlus m => Map x y -> Fin m x y
fromMap = Fin . fmap pure

toMap :: Fin m x y -> Map x (m y)
toMap (Fin m) = m

singleton :: MonadPlus m => x -> y -> Fin m x y
singleton x y = fromMap $ M.singleton x y

pairs :: Fin m x y -> [(x, m y)]
pairs (Fin m) = M.toList m

fromList :: (MonadPlus m, Ord x) => [(x, y)] -> Fin m x y
fromList xys = Fin $ M.fromListWith mplus $ Prelude.map (second pure) xys

fromSet :: (MonadPlus m, Ord x) => S.Set (x, y) -> Fin m x y
fromSet = fromList . S.toList

fun :: (MonadPlus m, Ord x) => S.Set x -> (x -> y) -> Fin m x y
fun xs f = fromList [(x, f x) | x <- S.toList xs]

id :: (MonadPlus m, Ord x) => S.Set x -> Fin m x x
id xs = fun xs Prelude.id

const :: (MonadPlus m, Ord x, Ord y) => S.Set x -> y -> Fin m x y
const xs y = fun xs $ Prelude.const y

(<+) :: (MonadPlus m, Ord x, Ord y) => Fin m x y -> Fin m x y -> Fin m x y
Fin f <+ Fin g = Fin $ f `M.union` g

(+>) :: (MonadPlus m, Ord x, Ord y) => Fin m x y -> Fin m x y -> Fin m x y
Fin f +> Fin g = Fin $ g `M.union` f

(<.>) :: (MonadPlus m, Ord x, Ord y, Ord z, Eq y, Eq (m z)) => Fin m y z -> Fin m x y -> Fin m x z
g <.> f =
  Fin $
    M.fromList $
      do
        (x, my) <- pairs f
        (y', mz) <- pairs g
        let mz' = do
              y <- my
              guard (y == y')
              mz
        guard (mz' /= Control.Applicative.empty)
        return (x, mz')

map :: (Functor m, Ord x') => (x -> x') -> (y -> y') -> Fin m x y -> Fin m x' y'
map f g (Fin m) = Fin $ M.map (fmap g) $ M.mapKeys f m

int :: (Ord x, MonadPlus m) => Fin m x y -> x -> m y
int (Fin m) x = fromMaybe mzero $ m M.!? x

dom :: Ord x => Fin m x y -> S.Set x
dom (Fin m) = S.fromList $ M.keys m

im :: (Ord x, Ord (m y)) => Fin m x y -> S.Set (m y)
im (Fin m) = S.fromList $ M.elems m

reachables :: (Eq x, Ord x) => Fin [] x x -> S.Set x -> S.Set x
reachables m = bfs mempty
  where
    bfs seen todo
      | S.null todo = seen
      | otherwise =
          let seen' = seen `S.union` todo
              todo' = S.fromList [x' | x <- S.toList todo, x' <- int m x] S.\\ seen'
           in bfs seen' todo'

reachable :: (Eq x, Ord x) => Fin [] x x -> x -> S.Set x
reachable m = reachables m . S.singleton

partApp :: (Eq x, Ord y) => Fin m (x, y) z -> x -> Fin m y z
partApp (Fin m) x =
  Fin $ M.mapKeys snd $ M.filterWithKey (\(x', _) _ -> x' == x) m

filter :: Ord x => (x -> m y -> Bool) -> Fin m x y -> Fin m x y
filter f (Fin m) = Fin $ M.filterWithKey f m
