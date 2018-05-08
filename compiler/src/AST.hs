module AST where

type Id     = String
type TypeId = Id

data Dec = TypeDec Type
         | VarDec Var
         | FunDec Fun
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

data Var = Var { vId   :: Id
               , vType :: Maybe TypeId
               , vExpr :: Expr
               } deriving (Show, Eq)

data Fun = Fun { fId   :: Id
               , fArgs :: [TypeField]
               , fType :: Maybe TypeId
               , fExpr :: Expr
               } deriving (Show, Eq)

data LValue = LId Id
            | LField LValue Id
            | LArray LValue Expr
            deriving (Show, Eq)

data BOp = Add | Sub | Mult | Div | Equal | NEqual
         | Gt | Lt | GTE | LTE | And | Or
         deriving (Show, Eq)

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

data Expr = LExpr LValue
          | Nil
          | ExprSeq [Expr]
          | NoValue
          | IExpr Integer
          | SExpr String
          | BExpr BOp Expr Expr
          | NExpr Expr
          | Record TypeId [RecordField]
          | Array TypeId Expr Expr
          | Assign LValue Expr
          | IfE Expr Expr Expr
          | If Expr Expr
          | While Expr Expr
          | For Expr Expr Expr
          | Break
          | Let [Dec] [Expr]
          | FCall Id [Expr]
          deriving (Show, Eq)

