module Tree
  ( Exp (..),
    Stm (..),
    BinOp (..),
    RelOp (..),
    seq,
  )
where

import Temp qualified
import Prelude hiding (seq)

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

seq :: [Stm] -> Stm
seq = foldr Seq (Exp $ Const 0)

instance Num Exp where
  (+) = BinOp Plus
  (-) = BinOp Minus
  (*) = BinOp Mul
  negate = BinOp Minus 0
  abs = undefined
  signum = undefined
  fromInteger = Const . fromInteger
