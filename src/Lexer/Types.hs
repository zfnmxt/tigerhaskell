module Lexer.Types where

import Data.Map (Map)
import Data.Set (Set)
import Lexer.Finite (Fin)

data FA m a s p = FA
  { delta :: Fin m (a, s) s,
    delta_e :: Fin m s s,
    start :: s,
    accept :: Set s,
    states :: Set s,
    alphabet :: Set a,
    payloads :: Map s p
  }
  deriving (Show)

type DFA = FA Maybe

type NFA = FA []

data Regex a
  = Sym a
  | Epsilon
  | Empty
  | (:|:) (Regex a) (Regex a)
  | (:::) (Regex a) (Regex a)
  | Star (Regex a)
  deriving (Show, Eq)

class Ord s => Node s where
  startNode :: s
  nextNode :: s -> s
  renameFun :: (Ord a, Functor m) => [FA m a s p] -> Int -> s -> s
