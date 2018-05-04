module AST where

type Id     = String
type TypeId = Id

data Dec = TyDec Type
         | VarDec Var
         | FunDec Fun

data Type   = Type { typeConst :: TypeId
                   , typeBody  :: TypeBody
                   }

data TypeBody  = DataConst
               | TypeRecord [TypeField]
               | TypeArray TypeId

data TypeField = TypeField { tfId   :: Id
                           , tfType :: TypeId
                           }

data Var = Var { varId   :: Id
               , varType :: Maybe TypeId
               , varExpr :: Expr
               }

data Fun = Fun { funId   :: Id
               , funArgs :: [TypeField]
               , funType :: Maybe TypeId
               , funExpr :: Expr
               }

data LValue = LId Id
            | LField LValue Id
            | LArray LValue [Expr]

data BOp = Plus | Sub | Mult | Div | Equal | NEqual
         | GT | LT | GTE | LTE | And | Or

data Assoc = Left | Right | None
type Prec = Int
data Op = Op BOp Assoc Prec

data RecordField = RecordField { rfId   :: Id
                               , rfType :: TypeId
                               }
data Expr = LExpr LValue
          | EmptyExpr
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









