module Tree where

import Temp qualified

data Exp
  = Const Int
  | Name Temp.Label
  | Temp Temp.Temp
  | BinOp BinOp Exp Exp
  | Mem Exp
  | Call Exp [Exp]
  | ESeq Stm Exp
  deriving (Show, Eq, Ord)

data Stm
  = Move Exp Exp
  | Exp Exp
  | Jump Exp [Temp.Label]
  | CJump RelOp Exp Exp Temp.Label Temp.Label
  | Seq Stm Stm
  | Label Temp.Label
  deriving (Show, Eq, Ord)

data BinOp
  = Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | LShift
  | RShift
  | ARShift
  | Xor
  deriving (Show, Eq, Ord)

data RelOp
  = Eq
  | NE
  | LT
  | GT
  | LE
  | GE
  | ULT
  | ULE
  | UGT
  | UGE
  deriving (Show, Eq, Ord)
