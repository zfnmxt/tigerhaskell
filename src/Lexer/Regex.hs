module Lexer.Regex where

import qualified Data.Set as S
import qualified Lexer.FA as FA
import qualified Lexer.Finite as F
import Lexer.Types

unions :: [Regex a] -> Regex a
unions = foldr (:|:) Empty

oneOf :: [a] -> Regex a
oneOf = unions . map Sym

maybe :: Regex a -> Regex a
maybe r = r :|: Epsilon

plus :: Regex a -> Regex a
plus r = r ::: Star r

lit :: [a] -> Regex a
lit = foldr (\a b -> Sym a ::: b) Epsilon

concat :: [Regex a] -> Regex a
concat = foldr (:::) Empty

toNFA :: (Node s, Ord a) => Regex a -> NFA a s
toNFA (Sym a) =
  FA
    { delta = F.fromList [((a, startNode), acceptNode)],
      delta_e = F.empty,
      start = startNode,
      accept = S.singleton acceptNode,
      states = S.fromList [startNode, acceptNode],
      alphabet = S.singleton a
    }
  where
    acceptNode = nextNode startNode
toNFA Epsilon =
  FA
    { delta = F.empty,
      delta_e = F.empty,
      start = startNode,
      accept = S.singleton startNode,
      states = S.singleton startNode,
      alphabet = S.empty
    }
toNFA Empty = (toNFA Epsilon) {accept = S.empty}
toNFA (r1 :|: r2) = toNFA r1 `FA.union` toNFA r2
toNFA (r1 ::: r2) = toNFA r1 `FA.concat` toNFA r2
toNFA (Star r) = FA.star $ toNFA r

toNFA_ :: Ord a => Regex a -> NFA a Int
toNFA_ = FA.simplifyStates . toNFA

toDFA :: (Node s, Ord a) => Regex a -> DFA a (S.Set s)
toDFA = FA.toDFA . toNFA

accepts :: Ord a => Regex a -> [a] -> Bool
accepts r = FA.accepts $ toNFA_ r
