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
lit = foldl1 (:::) . map Sym

concat :: [Regex a] -> Regex a
concat = foldl1 (:::)

toNFA :: Ord a => Regex a -> NFA a Int p
toNFA (Sym a) =
  FA
    { delta = F.fromList [((a, 0), 1)],
      delta_e = F.empty,
      start = 0,
      accept = S.singleton 1,
      states = S.fromList [0, 1],
      alphabet = S.singleton a,
      payloads = M.empty
    }
toNFA Epsilon =
  FA
    { delta = F.empty,
      delta_e = F.empty,
      start = 0,
      accept = S.singleton 0,
      states = S.singleton 0,
      alphabet = S.empty,
      payloads = M.empty
    }
toNFA Empty = (toNFA Epsilon) {accept = S.empty}
toNFA (r1 :|: r2) = toNFA r1 `FA.union` toNFA r2
toNFA (r1 ::: r2) = toNFA r1 `FA.concat` toNFA r2
toNFA (Star r) = FA.star $ toNFA r

toDFA :: (Ord a, Ord p) => Regex a -> DFA a Int p
toDFA = FA.toDFA . toNFA

accepts :: Ord a => Regex a -> [a] -> Bool
accepts r = FA.accepts $ toNFA r
