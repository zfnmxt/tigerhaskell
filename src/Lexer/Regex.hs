module Lexer.Regex where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lexer.FA as FA
import qualified Lexer.Finite as F
import Lexer.Types

unions :: [Regex a] -> Regex a
unions = foldl1 (:|:)

oneOf :: [a] -> Regex a
oneOf = unions . map Sym

maybe :: Regex a -> Regex a
maybe r = r :|: Epsilon

plus :: Regex a -> Regex a
plus r = r ::: Star r

lit :: [a] -> Regex a
lit = foldl (\b a -> Sym a ::: b) Epsilon

concat :: [Regex a] -> Regex a
concat = foldl1 (:::)

toNFA :: (Node s, Ord a) => Regex a -> NFA a s p
toNFA (Sym a) =
  FA
    { delta = F.fromList [((a, startNode), acceptNode)],
      delta_e = F.empty,
      start = startNode,
      accept = S.singleton acceptNode,
      states = S.fromList [startNode, acceptNode],
      alphabet = S.singleton a,
      payloads = M.empty
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
      alphabet = S.empty,
      payloads = M.empty
    }
toNFA Empty = (toNFA Epsilon) {accept = S.empty}
toNFA (r1 :|: r2) = toNFA r1 `FA.union` toNFA r2
toNFA (r1 ::: r2) = toNFA r1 `FA.concat` toNFA r2
toNFA (Star r) = FA.star $ toNFA r

toNFA_ :: Ord a => Regex a -> NFA a Int (S.Set p)
toNFA_ = FA.simplifyStates . toNFA

toDFA :: (Node s, Ord a, Ord p) => Regex a -> DFA a (S.Set s) (S.Set p)
toDFA = FA.toDFA . toNFA

accepts :: Ord a => Regex a -> [a] -> Bool
accepts r = FA.accepts $ toNFA_ r

-- flatten :: Regex a -> Regex a
-- flatten = unions . flatten'
--  where
--    flatten' (r1 :|: r2) = flatten' r1 ++ flatten' r2
--    flatten' (r1 ::: r2) = [flatten r1 ::: flatten r2]
--    flatten' r = [r]
