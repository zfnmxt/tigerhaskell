module Types where

import AST

data Ty = Int
        | String
        | Record [(Id, Ty)]
        | Array Ty
        | Nil
        | Unit
        deriving (Show)


instance Eq Ty where
  Nil == Record _        = True
  Record _ == Nil        = True
  Int == Int             = True
  String == String       = True
  Array x == Array y     = x == y
  Nil == Nil             = True
  Record xs == Record ys = xs == ys
  Unit == Unit           = True
  _ == _                 = False
