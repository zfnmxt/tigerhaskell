{-# LANGUAGE RecordWildCards #-}

module AST where

type Id     = String
type TypeId = Id

data Dec = TypeDec [Type]
         | VarDec VarDef
         | FunDec [FunDef]
         deriving (Show, Eq)

data Type   = Type { _typeId    :: TypeId -- typeC :: TypeId
                   , _typeBody  :: TypeBody
                   } deriving (Show, Eq)

data TypeBody  = DataConst TypeId
               | RecordType [TypeField]
               | ArrayType TypeId
               deriving (Show, Eq)

data TypeField = TypeField { _typeFieldId :: Id -- tfId :: Id
                           , _typeFieldType :: TypeId -- tfType  :: TypeId
                           } deriving (Show, Eq)
(|:) = TypeField

data VarDef = VarDef { _varDefId   :: Id -- vId   :: Id
                     , _varDefType :: Maybe TypeId -- vType :: Maybe TypeId
                     , _varDefExpr :: Expr-- vExpr :: Expr
                     } deriving (Show, Eq)

data Field = Field { _fieldId     :: Id
                   , _fieldType   :: TypeId
                   , _fieldEscape :: Bool
                   } deriving (Show, Eq)

data FunDef = FunDef { _funDefId :: Id --fId   :: Id
                     , _funDefArgs :: [Field] --fArgs :: [Field]
                     , _funDefType :: Maybe TypeId --fType :: Maybe TypeId
                     , _funDefExpr :: Expr --fExpr :: Expr
                     } deriving (Show, Eq)

data Var = SimpleVar Id
           | FieldVar Var Id
           | ArrayVar Var Expr
           deriving (Show, Eq)

data BOp = Add | Sub | Mult | Div | Equal | NEqual
         | Gt | Lt | GTE | LTE | And | Or
         deriving (Show, Eq, Enum)

data Assoc = L | R | None deriving (Show, Eq)
type Prec = Int

data RecordField = RecordField { _recordFieldId   :: Id -- rfId   :: Id
                               , _recordFieldExpr :: Expr -- rfExpr :: Expr
                               } deriving (Show, Eq)

(|.) = RecordField

data Expr = VExpr Var
          | NilExpr
          | ExprSeq [Expr]
          | UnitExpr
          | IExpr Integer
          | SExpr String
          | BExpr BOp Expr Expr
          | NExpr Expr
          | RecordExpr TypeId [RecordField]
          | ArrayExpr TypeId Expr Expr
          | Assign Var Expr
          | IfE Expr Expr Expr
          | If Expr Expr
          | While Expr Expr
          | For Expr Expr Expr
          | Break
          | Let [Dec] [Expr]
          | FCall Id [Expr]
          deriving (Show, Eq)

