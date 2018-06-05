{-# LANGUAGE DuplicateRecordFields #-}

module TypeChecker where

import AST
import Types
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Set (Set)
import qualified Data.Set as S
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

genError :: Show a => a -> String -> CheckerState b
genError x s = lift . Left $ "Error on term: " ++ show x ++ " with error msg: " ++ s

--------------------------------------------------------------------------------
-- Initial env
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Env functions
--------------------------------------------------------------------------------

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
    Just (FunEntry _ _) -> genError id "found function instead of var"
    Nothing             -> genError id "var not found"

lookupFun :: Id -> CheckerState VEnvEntry
lookupFun id = do
  (venv, _) <- S.get
  case M.lookup id venv of
    Just (VarEntry _ )           -> genError id "found var instead of function"
    Just (FunEntry argTys retTy) -> return $ FunEntry argTys retTy
    Nothing                      -> genError id "var not found"

insertTy :: TypeId -> Ty -> CheckerState ()
insertTy typeId ty = do
  (venv, tenv) <- S.get
  S.put (venv, M.insert typeId ty tenv)

lookupTy :: TypeId -> CheckerState Ty
lookupTy tId = do
  (_, tenv) <- S.get
  case M.lookup tId tenv of
    Just t  -> return t
    Nothing -> genError tId "type not found"

--------------------------------------------------------------------------------
-- Expression transformation and type checking
--------------------------------------------------------------------------------
transExprB :: Expr -> CheckerState TExprTy
transExprB Break = return ((), Unit)
transExprB expr  = transExpr expr

transExpr :: Expr -> CheckerState TExprTy
transExpr (NilExpr)   = return ((), Nil)
transExpr (IExpr _)   = return ((), Int)
transExpr (SExpr _)   = return ((), String)
transExpr Break       = genError Break "Breaks must be confined to for and while loops"
transExpr (VExpr var) = fmap (\ty -> ((), ty)) $ typeCheckVar var
transExpr expr@(Assign v exp) = do
  (_, vTy) <- transExpr (VExpr v)
  (_, eTy) <- transExpr exp
  if eTy |> vTy
  then return ((), Unit)
  else genError expr "type of expr doesn't match type of var"

transExpr expr@(ExprSeq exprs) = do
  tys <- mapM transExpr exprs
  return $ last tys

transExpr expr@(RecordExpr tId fields) = do
  recT       <- lookupTy tId
  case recT of
   Record _ (Just fieldTs) -> do
     checks <- nameTypeCheck
     if and checks
     then return ((), recT)
     else genError expr "field name or type don't match with declared record type"
       where nameTypeCheck =
               let f (field, (fname, ftype)) = do
                     (_, fexprT) <- transExpr (rfExpr field)
                     case ftype of
                       Record recTyId Nothing -> do
                                                 recTy <- lookupTy recTyId
                                                 return $ rfId field == fname && fexprT |> recTy
                       _                      -> return $ rfId field == fname && fexprT |> ftype
               in mapM f $ zip fields fieldTs
   _                ->
     genError expr "type of expr not a record type"

transExpr expr@(ArrayExpr tId n v) = do
  arrayT  <- lookupTy tId
  (_, nT) <- transExpr n
  (_, vT) <- transExpr v
  case arrayT of
    Array aId t -> if nT == Int
                   then do
                     t' <- case t of
                               Record recId Nothing -> lookupTy recId
                               _                    -> return t
                     if vT |> t'
                      then return ((), Array aId t')
                      else genError expr $ "v should have type " ++ show t
               else genError expr "n must have type int"
    _       -> genError expr "type of array expr isn't an array type"

transExpr nExpr@(NExpr expr) = do
  (_, eType) <- transExpr expr
  case eType of
    Int -> return ((), Int)
    _   -> genError nExpr "int required"

transExpr expr@(BExpr op l r)
  | op `elem` [Add, Sub, Mult, Div, And, Or] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int) -> return ((), Int)
       _          -> genError expr "int required"
  | op `elem` [Gt, Lt, GTE, LTE] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int)       -> return ((), Int)
       (String, String) -> return ((), Int)
       _                -> genError expr "ints or strings required"
  | op `elem` [Equal, NEqual] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     if lType |> rType || rType |> lType
     then return ((), Int)
     else genError expr $ "ints, strings, array, or recs required"
                                         ++ " left type: " ++ show lType ++ " right type: "
                                         ++ show rType

transExpr expr@(If cond body) = do
    (_, condT) <- transExpr cond
    (_, bodyT) <- transExpr body
    case condT of
      Int -> case bodyT of
                   Unit -> return ((), Unit)
                   _    -> genError expr "body of if expression must return no value"
      _   -> genError expr "cond of if expression must have type int"

transExpr expr@(IfE cond body1 body2) = do
    (_, condT)  <- transExpr cond
    (_, body1T) <- transExpr body1
    (_, body2T) <- transExpr body2
    case condT of
      Int -> if body1T == body2T
             then return ((), body1T)
             else genError expr "both expressions in IFE must have the same type"
      _        -> genError expr "cond of if expression must have type int"

transExpr expr@(While cond body) = do
    (_, condT) <- transExpr cond
    (_, bodyT) <- transExprB body
    case condT of
      Int -> case bodyT of
                Unit -> return ((), Unit)
                _    -> genError expr "body of while expression must return no value"
      _   -> genError expr "cond of while expression must have type int"

transExpr expr@(For (Assign (SimpleVar x) min) max body) = do
  (_, minT)   <- transExpr min
  (_, maxT)   <- transExpr max
  oldEnv <- S.get
  transDec (VarDec (VarDef x Nothing min))
  (_, bodyT)  <- transExprB body
  case (minT, maxT) of
    (Int, Int) -> case bodyT of
                     Unit -> return ((), Unit)
                     _    -> genError expr "body of for-exprresion must return no value"
    _          -> genError expr "bounds of for-expression must have type int"

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
  else S.put oldEnv >> genError expr "args don't match argtype of function"

transExpr UnitExpr = return ((), Unit)
transExpr expr = genError expr "pattern match failed"

--------------------------------------------------------------------------------
-- Var typechecking
--------------------------------------------------------------------------------

typeCheckVar :: Var -> CheckerState Ty
typeCheckVar var = do
  case var of
    SimpleVar v -> do
      (vEnv, _) <- S.get
      case M.lookup v vEnv of
        Just (VarEntry vType) -> return vType
        Just (FunEntry _ _  ) -> genError var "function with same name exists"
        _                     -> genError var "undefined var"

    FieldVar r field -> do
      rTy <- typeCheckVar r
      case rTy of
        Record _ (Just fieldTypes) ->
          case L.lookup field fieldTypes of
            Nothing    -> genError var "field does not exist in record"
            Just fType -> return fType
        _                 -> genError var "non-record type"

    ArrayVar a _ -> do
      arrayTy <- typeCheckVar a
      case arrayTy of
        Array _ t -> return t
        _         -> genError var "not an array type"

--------------------------------------------------------------------------------
-- Declaration type checking
--------------------------------------------------------------------------------
duplicates :: Ord a => [a] -> Bool
duplicates xs = length xs /= length (S.fromList xs)

transDec :: Dec -> CheckerState ()
transDec d = transDecHeader d >> transDecBody d

transDecHeader :: Dec -> CheckerState ()
transDecHeader (VarDec v) = return ()
transDecHeader (TypeDec tys) =
    if duplicates names
    then genError tys "has duplicate type names"
    else mapM_ addTy tys
    where names = map typeC tys
          addTy t@(Type tyC (DataConst tyId))      = lookupTy tyId >>= \t -> insertTy tyC t
          addTy t@(Type tyC (RecordType tyFields)) = insertTy tyC (Record tyC Nothing)
          addTy t@(Type tyC (ArrayType tyId))      = lookupTy tyId >>= \t -> insertTy tyC (Array tyC t)

transDecHeader (FunDec fs) =
  if duplicates names
  then genError fs "has duplicate function names"
  else mapM_ addF fs
  where names = map fId fs
        addF f@(FunDef funId args resType body) = do
          argTyPairs <- getFieldTys args
          let argTys =  map snd argTyPairs
          resTy      <- case resType of
                         Nothing       -> return Unit
                         Just resType' -> lookupTy resType'
          insertFun funId argTys resTy
          return ()

transDecBody :: Dec -> CheckerState ()
transDecBody (VarDec v) =
  case vType v of
    Nothing -> do
      (_, t) <- transExpr (vExpr v)
      if t == Nil
      then genError v "expressions of type nil must be constrained by a record type"
      else insertVar (vId v) t
    Just tId  -> do
      (_, t)  <- transExpr (vExpr v)
      t'      <- lookupTy tId
      case t of
        Nil -> case t' of
                 Record _ _ -> insertVar (vId v) t'
                 _          -> genError v
                               "expressions of type nil must be constrained to records"
        _   -> if t == t'
               then insertVar (vId v) t'
               else genError v "var type does not match expression type"

transDecBody (TypeDec tys) = mapM_ addTy tys
    where addTy t@(Type tyC (RecordType tyFields)) = do
            fieldTys <- getFieldTys tyFields
            insertTy tyC $ Record tyC (Just fieldTys)
          addTy _ = return ()

transDecBody (FunDec fs) = mapM addF fs >> return ()
  where addF f@(FunDef funId args resType body) = do
            FunEntry argTs resTy <- lookupFun funId
            oldEnv <- S.get
            let argTyPairs = zipWith (\(TypeField id _) ty -> (id, ty)) args argTs
            mapM (\(id, ty) -> insertVar id ty) argTyPairs
            (_, tBody) <- transExpr body
            if tBody == resTy
            then S.put oldEnv
            else genError f "res type doesn't match the type of the body"

getFieldTys :: [TypeField] -> CheckerState [(Id, Ty)]
getFieldTys typeFields = do
  (venv, tenv) <- S.get
  mapM (f tenv) typeFields
  where f tenv (TypeField name fieldT) =
         case M.lookup fieldT tenv of
           Nothing -> genError typeFields "typefield has invalid fields"
           Just ty -> return (name, ty)
