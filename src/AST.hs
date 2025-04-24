module AST
  ( UntypedExp,
    UntypedTy,
    UntypedDec,
    UntypedFunDec,
    UntypedField,
    UntypedVar,
    TypedExp,
    Var (..),
    Exp (..),
    Dec (..),
    Ty (..),
    Oper (..),
    FunDec (..),
    Field (..),
  )
where

import Text.Megaparsec.Pos

data Symbol = Symbol String Int deriving (Show, Ord)

instance Eq Symbol where
  Symbol _ i1 == Symbol _ i2 = i1 == i2

type UntypedVar = Var String

type UntypedExp = Exp String

type UntypedTy = Ty String

type UntypedDec = Dec String

type UntypedFunDec = FunDec String

type UntypedField = Field String

type TypedExp = Exp Symbol

data Var s
  = SimpleVar s SourcePos
  | FieldVar (Var s) s SourcePos
  | SubscriptVar (Var s) (Exp s) SourcePos
  deriving (Show, Eq, Ord)

data Exp s
  = VarExp (Var s)
  | NilExp
  | IntExp Integer SourcePos
  | StringExp String SourcePos
  | CallExp String [Exp s] SourcePos
  | OpExp (Exp s) Oper (Exp s) SourcePos
  | RecordExp s [(s, Exp s, SourcePos)] SourcePos
  | SeqExp [(Exp s, SourcePos)]
  | AssignExp (Var s) (Exp s) SourcePos
  | IfExp (Exp s) (Exp s) (Maybe (Exp s)) SourcePos
  | WhileExp (Exp s) (Exp s) SourcePos
  | ForExp s (Exp s) (Exp s) (Exp s) SourcePos
  | BreakExp SourcePos
  | LetExp [Dec s] (Exp s) SourcePos
  | ArrayExp s (Exp s) (Exp s) SourcePos
  deriving (Show, Eq, Ord)

-- We don't allow lists of declaration for mutual recursion here, unlike the
-- spec in the book.  Anything can be mutually recursive with anything else in
-- scope.
data Dec s
  = FunctionDec (FunDec s)
  | VarDec s (Maybe (s, SourcePos)) (Exp s) SourcePos
  | TypeDec (s, Ty s, SourcePos)
  deriving (Show, Eq, Ord)

data Ty s
  = NameTy s SourcePos
  | RecordTy [Field s]
  | ArrayTy s SourcePos
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

data FunDec s = FunDec
  { funName :: s,
    funParams :: [Field s],
    funResult :: Maybe (s, SourcePos),
    funBody :: Exp s,
    funLoc :: SourcePos
  }
  deriving (Show, Eq, Ord)

data Field s = Field s s SourcePos
  deriving (Show, Eq, Ord)
