module AST where

type Id     = String
type TypeId = Id

data Dec = TypeDec [Type]
         | VarDec VarDef
         | FunDec [FunDef]
         deriving (Show, Eq)

data Type   = Type { typeC :: TypeId
                   , typeBody  :: TypeBody
                   } deriving (Show, Eq)

data TypeBody  = DataConst TypeId
               | RecordType [TypeField]
               | ArrayType TypeId
               deriving (Show, Eq)

data TypeField = TypeField { tfId :: Id
                           , tfType  :: TypeId
                           } deriving (Show, Eq)
(|:) = TypeField

data VarDef = VarDef { vId   :: Id
                     , vType :: Maybe TypeId
                     , vExpr :: Expr
                     } deriving (Show, Eq)

data FunDef = FunDef { fId   :: Id
                     , fArgs :: [TypeField]
                     , fType :: Maybe TypeId
                     , fExpr :: Expr
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
data Op = Op { operator   :: BOp
             , assoc      :: Assoc
             , precedence :: Prec
             } deriving (Show, Eq)

data RecordField = RecordField { rfId   :: Id
                               , rfExpr :: Expr
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

