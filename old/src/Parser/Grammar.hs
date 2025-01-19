module Parser.Grammar where

import Data.Char
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Parser.Types
import Util

mkGrammar :: [(Char, String)] -> Grammar Char
mkGrammar = map f
  where
    f (c, ss)
      | isUpper c = (N c, map (\s -> if isUpper s then N s else T s) ss)
      | otherwise = error "mkGrammar"

isTerminal :: Symbol a -> Bool
isTerminal T {} = True
isTerminal _ = False

symbols :: Ord a => Grammar a -> Set (Symbol a)
symbols = foldMap $ \(l, r) -> S.fromList $ l : r

terminals :: Ord a => Grammar a -> Set (Symbol a)
terminals = S.filter isTerminal . symbols

nonterminals :: Ord a => Grammar a -> Set (Symbol a)
nonterminals = S.filter (not . isTerminal) . symbols

nullable :: (Eq a, Ord a) => Grammar a -> Symbol a -> Bool
nullable _ (T _) = False
nullable g x = x `S.member` nullable_set
  where
    nullable_set = converge mempty $ \set -> foldr f set g
    f (x', ys) set'
      | all (`S.member` set') ys = S.insert x' set'
      | otherwise = set'

nullable_ :: (Eq a, Ord a) => Grammar a -> [Symbol a] -> Bool
nullable_ g = all $ nullable g

first :: (Eq a, Ord a) => Grammar a -> Symbol a -> Set (Symbol a)
first _ (T a) = S.singleton $ T a
first g x = fromMaybe mempty $ first_map M.!? x
  where
    init_map = M.fromList $ map (\x' -> (x', S.singleton x')) $ S.toList $ terminals g
    first_map = converge init_map $ \m -> foldr f m g
    f (_, []) m' = m'
    f (x', (y : ys)) m' =
      let m'' = M.insertWith (<>) x' (m' !? y) m'
       in if nullable g y then f (x', ys) m'' else m''

first_ :: (Eq a, Ord a) => Grammar a -> [Symbol a] -> Set (Symbol a)
first_ _ [] = mempty
first_ g [x] = first g x
first_ g (x : xs)
  | nullable g x = first g x <> first_ g xs
  | otherwise = first g x

(!?) :: Ord k => Monoid v => Map k v -> k -> v
m !? k = fromMaybe mempty $ m M.!? k

follow :: (Eq a, Ord a) => Grammar a -> Symbol a -> Set (Symbol a)
follow g x = fromMaybe mempty $ follow_map M.!? x
  where
    follow_map = converge mempty $ \m -> foldr f m g
    follow' x' y [] m' = M.insertWith (<>) y ((m' !? y) <> (m' !? x')) m'
    follow' x' y (y' : ys) m' =
      let m'' = M.insertWith (<>) y ((m' !? y) <> first g y') m'
       in if nullable g y'
            then follow' x' y ys m''
            else m''
    f (_, []) m' = m'
    f (x', (y : ys)) m' = f (x', ys) $ follow' x' y ys m'

predParseTable :: Ord a => Grammar a -> Map (Symbol a, Symbol a) (Set (Rule a))
predParseTable g =
  M.fromListWith (<>) $
    [((x, t), S.singleton r) | r@(x, y) <- g, t <- toList $ first_ g y]
      ++ [((x, t), S.singleton r) | r@(x, y) <- g, nullable_ g y, t <- toList $ follow g x]

isLL1 :: Ord a => Grammar a -> Bool
isLL1 = all (\s -> length s <= 1) . M.elems . predParseTable
