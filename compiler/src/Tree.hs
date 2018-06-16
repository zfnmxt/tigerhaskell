module Tree where

import qualified Temp as T

data Exp = Const Int
         | Name T.Label
         | IReg T.Temp
         | BinOp BinOp Exp Exp
         | Mem Exp
         | Call Exp [Exp]
         | ESeq Stm Exp

data Stm = Move Exp Exp
         | StmExp Exp
         | Jump Exp [T.Label]
         | CJump { _cJumpRelOp :: RelOp
                 , _cJumpExp1  :: Exp
                 , _cJumpExp2  :: Exp
                 , _cJumpTrue  :: T.Label
                 , _cJumpFalse :: T.Label
                 }
         | Seq Stm Stm
         | StmLabel T.Label

data BinOp = Plus | Sub | Mul | Div | And | Or
             | LShift | RShift | ARShift | XOR
data RelOp = Equal | NEqual | Lt | Gt | GTE | LTE
             | ULt | ULTE | UGTE | UGt



