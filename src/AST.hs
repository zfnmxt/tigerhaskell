module AST
  ( Var (..),
    Exp (..),
    Dec (..),
    Ty (..),
    Oper (..),
    FunDec (..),
    Field (..),
  )
where

import Data.Loc

newtype Symbol = Symbol String
  deriving (Show, Eq, Ord)

data Var
  = SimpleVar Symbol SrcLoc
  | FieldVar Var Symbol SrcLoc
  | SubscriptVar Var Exp SrcLoc
  deriving (Show, Eq, Ord)

data Exp
  = VarExp Var
  | NilExp
  | IntExp Integer SrcLoc
  | StringExp String SrcLoc
  | CallExp Symbol [Exp] SrcLoc
  | OpExp Exp Oper Exp SrcLoc
  | RecordExp [(Symbol, Exp, SrcLoc)] Symbol SrcLoc
  | SeqExp [(Exp, SrcLoc)]
  | AssignExp Var Exp SrcLoc
  | IfExp Exp Exp (Maybe Exp) SrcLoc
  | WhileExp Exp Exp SrcLoc
  | ForExp Symbol Bool Exp Exp Exp SrcLoc
  | BreakExp SrcLoc
  | LetExp [Dec] Exp SrcLoc
  | ArrayExp Symbol Exp Exp SrcLoc
  deriving (Show, Eq, Ord)

data Dec
  = FunctionDec [FunDec]
  | VarDec Symbol Bool (Maybe (Symbol, SrcLoc)) Exp SrcLoc
  | TypeDec [(Symbol, Ty, SrcLoc)]
  deriving (Show, Eq, Ord)

data Ty
  = NameTy Symbol SrcLoc
  | RecordTy [Field]
  | ArrayTy Symbol SrcLoc
  deriving (Show, Eq, Ord)

data Oper
  = PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  deriving (Show, Eq, Ord)

data FunDec = FunDec
  { funName :: Symbol,
    funParams :: [Field],
    funResult :: Maybe (Symbol, SrcLoc),
    funBody :: Exp,
    funLoc :: SrcLoc
  }
  deriving (Show, Eq, Ord)

data Field = Field Symbol Bool Symbol SrcLoc
  deriving (Show, Eq, Ord)
