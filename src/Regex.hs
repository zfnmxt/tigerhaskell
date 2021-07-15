module Regex where

data Regex a
  = Sym a
  | (:||) (Regex a) (Regex a)
  | (:::) (Regex a) (Regex a)
  | Empty
  | (:*) (Regex a)
  deriving (Show, Eq)
