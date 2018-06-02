{-# LANGUAGE DuplicateRecordFields #-}

module TypeChecker where

import AST
import Types
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.State.Lazy (lift)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, execStateT)
import qualified Data.List as L


data VEnvEntry = VarEntry {ty :: Ty}
               | FunEntry {argTys :: [Ty], retTy :: Ty}
               deriving (Show, Eq)

type TExpr = ()
type TExprTy = (TExpr, Ty)
type VEnv = Map Id VEnvEntry
type TEnv = Map TypeId Ty
type Env  = (VEnv, TEnv)

type TError = String
type CheckerState = StateT Env (Either TError)

insertVar :: Id -> Ty -> CheckerState ()
insertVar id t = do
  (venv, tenv) <- S.get
  S.put (M.insert id (VarEntry t) venv, tenv)

insertFun :: Id -> [Ty] -> Ty -> CheckerState ()
insertFun id argsTy resTy = do
  (venv, tenv) <- S.get
  S.put (M.insert id (FunEntry argsTy resTy) venv, tenv)

lookupVar :: Id -> CheckerState VEnvEntry
lookupVar id = do
  (venv, _) <- S.get
  case M.lookup id venv of
    Just (VarEntry t)   -> return (VarEntry t)
    Just (FunEntry _ _) -> lift . Left $ genError id "found function instead of var"
    Nothing             -> lift . Left $ genError id "var not found"

lookupFun :: Id -> CheckerState VEnvEntry
lookupFun id = do
  (venv, _) <- S.get
  case M.lookup id venv of
    Just (VarEntry _ )           -> lift . Left $ genError id "found var instead of function"
    Just (FunEntry argTys retTy) -> return (FunEntry argTys retTy)
    Nothing                      -> lift . Left $ genError id "var not found"

lookupTy :: TypeId -> CheckerState Ty
lookupTy tId = do
  (_, tenv) <- S.get
  case M.lookup tId tenv of
    Just t  -> return t
    Nothing -> lift . Left $ genError tId "type not found"

insertTy :: TypeId -> Ty -> CheckerState ()
insertTy typeId ty = do
  (venv, tenv) <- S.get
  S.put (venv, M.insert typeId ty tenv)

genError :: Show a => a -> String -> TError
genError expr s = "Error in expr:\n" ++ show expr ++ "\n with error msg: " ++ s
                          --
baseTEnv = M.fromList [("int", Int), ("string", String)]
baseVEnv = M.fromList [ ("print",     FunEntry [String] Unit)
                      , ("flush",     FunEntry [] Unit)
                      , ("getchar",   FunEntry [] String)
                      , ("ord",       FunEntry [String] Int)
                      , ("chr",       FunEntry [Int] String)
                      , ("size",      FunEntry [String] Int)
                      , ("substring", FunEntry [String, Int, Int] String)
                      , ("concat",    FunEntry [String, String] String)
                      , ("not",       FunEntry [Int] Int)
                      , ("exit",      FunEntry [Int] Unit)
                      ]

initEnv = (baseVEnv, baseTEnv)

transExpr :: Expr -> CheckerState TExprTy
transExpr (NilExpr) = return ((), Nil)
transExpr (IExpr _) = return ((), Int)
transExpr (SExpr _) = return ((), String)
transExpr (Break)   = return ((), Unit)
transExpr (VExpr var) = fmap (\ty -> ((), ty)) $ typeCheckVar var

transExpr nExpr@(NExpr expr) = do
  (_, eType) <- transExpr expr
  case eType of
    Int -> return ((), Int)
    _   -> lift . Left $ genError nExpr "int required"

transExpr expr@(BExpr op l r)
  | op `elem` [Add, Sub, Mult, Div, And, Or] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int) -> lift . Right $ ((), Int)
       _          -> lift . Left $ genError expr "int required"
  | op `elem` [Equal, NEqual, Gt, Lt, GTE, LTE] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int)       -> lift . Right $ ((), Int)
       (String, String) -> lift . Right $ ((), Int)
       _                -> lift . Left $ genError expr "ints or strings required"

transExpr expr@(If cond body) = do
    (_, condT) <- transExpr cond
    (_, bodyT) <- transExpr body
    case condT of
      Int -> case bodyT of
                   Unit -> return ((), Unit)
                   _    -> lift . Left $ genError expr "body of if expression must return no value"
      _   -> lift . Left $ genError expr "cond of if expression must have type int"

transExpr expr@(IfE cond body1 body2) = do
    (_, condT)  <- transExpr cond
    (_, body1T) <- transExpr body1
    (_, body2T) <- transExpr body2
    case condT of
      Int -> case (body1T, body2T) of
                (Unit, Unit) -> return ((), Unit)
                _            -> lift . Left $ genError expr "body of if expression must return no value"
      _        -> lift . Left $ genError expr "cond of if expression must have type int"
transExpr expr@(While cond body) = do
    (_, condT) <- transExpr cond
    (_, bodyT) <- transExpr body
    case condT of
      Int -> case bodyT of
                Unit -> return ((), Unit)
                _    -> lift . Left $ genError expr "body of while expression must return no value"
      _   -> lift . Left $ genError expr "cond of while expression must have type int"

transExpr expr@(For (Assign (SimpleVar x) min) max body) = do
  (_, minT)   <- transExpr min
  (_, maxT)   <- transExpr max
  oldEnv <- S.get
  transDec (VarDec (VarDef x Nothing min))
  (_, bodyT)  <- transExpr body
  case (minT, maxT) of
    (Int, Int) -> case bodyT of
                     Unit -> return ((), Unit)
                     _    -> lift . Left $ genError expr "body of for-exprresion must return no value"
    _          -> lift . Left $ genError expr "bounds of for-expression must have type int"

transExpr expr@(Let decs exprs) = do
  oldEnv <- S.get
  mapM transDec decs
  exprTys <- mapM transExpr exprs
  S.put oldEnv
  return $ last exprTys

transExpr expr@(FCall f args) = do
  oldEnv <- S.get
  (FunEntry argTys retTy) <- lookupFun f
  passedArgTys <- mapM (\a -> snd <$> transExpr a) args
  if passedArgTys == argTys
  then S.put oldEnv >> return ((), retTy)
  else S.put oldEnv >> (lift . Left $ genError expr "args don't match argtype of function")


typeCheckVar :: Var -> CheckerState Ty
typeCheckVar var = do
  case var of
    SimpleVar v -> do
      (vEnv, _tEnv) <- S.get
      case M.lookup v vEnv of
        Just (VarEntry vType) -> return vType
        Just (FunEntry _ _  ) -> lift . Left $ genError var "function with same name exists"
        _                     -> lift . Left $ genError var "undefined var"

    FieldVar r field -> do
      Record fieldTypes <- typeCheckVar r
      case L.lookup field fieldTypes of
        Nothing    -> lift . Left $ genError var "field does not exist in record"
        Just fType -> return fType

    ArrayVar a _ -> do
      arrayTy <- typeCheckVar a
      case arrayTy of
        Array t -> return t
        _       -> lift . Left $ genError var "not an array type"

transDec :: Dec -> CheckerState ()
transDec (VarDec v) =
  case vType v of
    Nothing -> do
      (_, t) <- transExpr (vExpr v)
      if t == Nil
      then lift . Left $ genError v "expressions of type nil must be constrained by a record type"
      else insertVar (vId v) t
    Just tId  -> do
      (_, t)  <- transExpr (vExpr v)
      t'      <- lookupTy tId
      case t of
        Nil -> case t' of
                 Record _ -> insertVar (vId v) t'
                 _        -> lift . Left $ genError v "expressions of type nil must be constrained to records"
        _   -> if t == t'
               then insertVar (vId v) t'
               else lift . Left $ genError v "var type does not match expression type"
transDec (TypeDec tys) = mapM addTy tys >> return ()
    where addTy t@(Type tyC tyBody) =
            case tyBody of
              DataConst tyId      -> lookupTy tyId >>= \t -> insertTy tyC t
              RecordType tyFields -> typeFieldCheck tyFields (\tyPairs -> insertTy tyC (Record tyPairs))
              ArrayType tyId      -> lookupTy tyId >>= \t -> insertTy tyC (Array t)

transDec (FunDec fs) = mapM addF fs >> return ()
  where addF f@(FunDef funId args resType body) = do
          argTyPairs <- typeFieldCheck args return
          let argTys = map snd argTyPairs
          resTy <- case resType of
                     Nothing       -> return Unit
                     Just resType' -> lookupTy resType'
          insertFun funId argTys resTy
          (venv',_) <- S.get
          mapM (\(id, ty) -> insertTy id ty) argTyPairs
          (_, tBody) <- transExpr body
          if tBody == resTy
          then return ()
          else lift . Left $ genError f "res type doesn't match the type of the body"

typeFieldCheck :: [TypeField] -> ([(Id, Ty)] -> CheckerState a) -> CheckerState a
typeFieldCheck tyFields f = do
  (venv, tenv) <- S.get
  let g (TypeField id fTy) = (\ty -> (id, ty)) <$> M.lookup fTy tenv
  case mapM g tyFields of
    Just tyPairs -> f tyPairs
    Nothing      -> lift . Left $ genError tyFields "typefield has invalid fields"













