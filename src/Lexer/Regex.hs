module Lexer.Regex where

import qualified Data.Set as S
import qualified Lexer.FA as FA
import qualified Lexer.Finite as F
import Lexer.Types

oneOf :: [a] -> Regex a
oneOf = foldr (\a b -> b :|: Sym a) Empty

maybe :: Regex a -> Regex a
maybe r = r :|: Epsilon

plus :: Regex a -> Regex a
plus r = r ::: Star r

lit :: [a] -> Regex a
lit = foldr (\a b -> Sym a ::: b) Epsilon

toNFA :: Ord a => Regex a -> NFA a String
toNFA (Sym a) =
  FA
    { delta = F.fromList [((a, "start"), "accept")],
      delta_e = F.empty,
      start = "start",
      accept = S.singleton "accept",
      states = S.fromList ["start", "accept"],
      alphabet = S.singleton a
    }
toNFA Epsilon =
  FA
    { delta = F.empty,
      delta_e = F.empty,
      start = "start",
      accept = S.singleton "start",
      states = S.singleton "start",
      alphabet = S.empty
    }
toNFA Empty = (toNFA Epsilon) {accept = S.empty}
toNFA (r1 :|: r2) = toNFA r1 `FA.union` toNFA r2
toNFA (r1 ::: r2) = toNFA r1 `FA.concat` toNFA r2
toNFA (Star r) = FA.star $ toNFA r

accepts :: Ord a => Regex a -> [a] -> Bool
accepts r = FA.accepts $ toNFA r

whitespace :: Regex Char
whitespace = Star $ oneOf [' ', '\t', '\n']

digit :: Regex Char
digit = oneOf ['0' .. '9']
