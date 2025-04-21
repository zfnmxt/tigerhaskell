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

import Text.Megaparsec.Pos

data Var
  = SimpleVar String SourcePos
  | FieldVar Var String SourcePos
  | SubscriptVar Var Exp SourcePos
  deriving (Show, Eq, Ord)

data Exp
  = VarExp Var
  | NilExp
  | IntExp Integer SourcePos
  | StringExp String SourcePos
  | CallExp String [Exp] SourcePos
  | OpExp Exp Oper Exp SourcePos
  | RecordExp String [(String, Exp, SourcePos)] SourcePos
  | SeqExp [(Exp, SourcePos)]
  | AssignExp Var Exp SourcePos
  | IfExp Exp Exp (Maybe Exp) SourcePos
  | WhileExp Exp Exp SourcePos
  | ForExp String Exp Exp Exp SourcePos
  | BreakExp SourcePos
  | LetExp [Dec] Exp SourcePos
  | ArrayExp String Exp Exp SourcePos
  deriving (Show, Eq, Ord)

data Dec
  = FunctionDec FunDec
  | VarDec String (Maybe (String, SourcePos)) Exp SourcePos
  | TypeDec (String, Ty, SourcePos)
  deriving (Show, Eq, Ord)

data Ty
  = NameTy String SourcePos
  | RecordTy [Field]
  | ArrayTy String SourcePos
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
  { funName :: String,
    funParams :: [Field],
    funResult :: Maybe (String, SourcePos),
    funBody :: Exp,
    funLoc :: SourcePos
  }
  deriving (Show, Eq, Ord)

data Field = Field String String SourcePos
  deriving (Show, Eq, Ord)
