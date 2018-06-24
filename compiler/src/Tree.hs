module Tree where

import qualified Temp as T

data TreeExp = Const Int
             | Name T.Label
             | IReg T.Temp
             | BinOp BinOp TreeExp TreeExp
             | Mem TreeExp
             | Call TreeExp [TreeExp]
             | ESeq TreeStm TreeExp
             deriving (Show, Eq)

data TreeStm = Move TreeExp TreeExp
             | StmExp TreeExp
             | Jump TreeExp [T.Label]
             | CJump { _cJumpRelOp :: RelOp
                     , _cJumpExp1  :: TreeExp
                     , _cJumpExp2  :: TreeExp
                     , _cJumpTrue  :: T.Label
                     , _cJumpFalse :: T.Label
                     }
             | Seq TreeStm TreeStm
             | StmLabel T.Label
             deriving (Show, Eq)

(>>$) :: TreeStm -> TreeExp -> TreeExp
(>>$) = ESeq

infixl 5 >>$

(>>>) :: TreeStm -> TreeStm -> TreeStm
(>>>) = Seq

infixl 5 >>>

data BinOp = Plus | Sub | Mul | Div | And | Or
             | LShift | RShift | ARShift | XOR
             deriving (Show, Eq)
data RelOp = Equal | NEqual | Lt | Gt | GTE | LTE
             | ULt | ULTE | UGTE | UGt
             deriving (Show, Eq)

