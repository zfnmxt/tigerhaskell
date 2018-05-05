module AST where

type Id     = String
type TypeId = Id

data Dec = TypeDec Type
         | VarDec Var
         | FunDec Fun
         deriving (Show)

data Type   = Type { typeC :: TypeId
                   , typeBody  :: TypeBody
                   } deriving (Show)

data TypeBody  = DataConst TypeId
               | TypeRecord [TypeField]
               | TypeArray TypeId
               deriving (Show)

data TypeField = TypeField { tfId   :: Id
                           , tfType :: TypeId
                           } deriving (Show)

data Var = Var { vId   :: Id
               , vType :: Maybe TypeId
               , vExpr :: Expr
               } deriving (Show)

data Fun = Fun { fId   :: Id
               , fArgs :: [TypeField]
               , fType :: Maybe TypeId
               , fExpr :: Expr
               } deriving (Show)

data LValue = LId Id
            | LField LValue Id
            | LArray LValue Expr
            deriving (Show)

data BOp = Plus | Sub | Mult | Div | Equal | NEqual
         | GT | LT | GTE | LTE | And | Or
         deriving (Show)

data Assoc = L | R | None deriving (Show)
type Prec = Int
data Op = Op BOp Assoc Prec
          deriving (Show)

data RecordField = RecordField { rfId   :: Id
                               , rfExpr :: Expr
                               } deriving (Show)
data Expr = LExpr LValue
          | Nil
          | ExprSeq [Expr]
          | NoValue
          | IExpr Integer
          | SExpr String
          | BExpr Op Expr Expr
          | NExpr Expr
          | Record TypeId [RecordField]
          | Array TypeId Expr Expr
          | Assign LValue Expr
          | IfE Expr Expr Expr
          | If Expr Expr
          | While Expr Expr
          | For Expr Expr Expr
          | Break
          | Let [Dec] Expr
          | FExpr Id [Expr]
          deriving (Show)

