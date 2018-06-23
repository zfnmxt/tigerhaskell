{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Semant where

import           AST
import           Frame                          as F
import           STEnv
import           Temp                           (Label (..), Temp (..))
import           Translate                      as T
import           Translate
import           Types

import           Control.Monad.State.Lazy       (lift)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, execStateT,
                                                 runStateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.List                      (replicate)
import qualified Data.List                      as L
import           Data.Map.Lazy                  (Map)
import qualified Data.Map.Lazy                  as M
import           Data.Set                       (Set)
import qualified Data.Set                       as S

data TExpr = TExpr { _tExpr   :: TransExp
                   , _tExprTy :: Ty
                   } deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Expression transformation and type checking
--------------------------------------------------------------------------------
transExprB :: Expr -> STEnvT TExpr
transExprB Break = return $ TExpr NoExp Unit
transExprB expr  = transExpr expr

transExpr :: Expr -> STEnvT TExpr
transExpr (NilExpr)   = return $ TExpr NoExp Nil
transExpr (IExpr x)   = do
  tExp <- iExpr x
  return $ TExpr tExp Int
transExpr (SExpr _)   = return $ TExpr NoExp String
transExpr Break       = genError Break "Breaks must be confined to for and while loops"
transExpr (VExpr var) =
  case var of
    SimpleVar id -> do
      ty                <- typeCheckVar var
      VarEntry{..}      <- lookupVar id
      cLevel            <- getLevel
      tExp              <- simpleVar _varEntryAccess cLevel
      return $ TExpr tExp ty
    ArrayVar a iExpr -> do
      ty                 <- typeCheckVar var
      (TExpr aTrans aTy) <- transExpr (VExpr a)
      (TExpr iTrans iTy) <- transExpr iExpr
      case iTy of
        Int -> do
               tExp <- arrayVar aTrans iTrans
               return $ TExpr tExp ty
        _   -> genError var "array index must have type int"
    FieldVar r f -> do
      ty                 <- typeCheckVar var
      rTy                <- typeCheckVar r
      (TExpr rTrans rTy) <- transExpr (VExpr r)
      tExp               <- recordVar rTy rTrans f
      return $ TExpr tExp ty

transExpr expr@(Assign v exp) = do
  TExpr _ vTy <- transExpr (VExpr v)
  TExpr _ eTy <- transExpr exp
  if eTy |> vTy
  then return $ TExpr NoExp Unit
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
     then return $ TExpr NoExp recT
     else genError expr "field name or type don't match with declared record type"
       where nameTypeCheck =
               let f (RecordField{..}, (fname, ftype)) = do
                     TExpr _ fexprT <- transExpr _recordFieldExpr
                     case ftype of
                       Record recTyId Nothing -> do
                                                 recTy <- lookupTy recTyId
                                                 return $ _recordFieldId == fname && fexprT |> recTy
                       _                      -> return $ _recordFieldId == fname && fexprT |> ftype
               in mapM f $ zip fields fieldTs
   _                ->
     genError expr "type of expr not a record type"

transExpr expr@(ArrayExpr tId n v) = do
  arrayT  <- lookupTy tId
  TExpr _ nT <- transExpr n
  TExpr _ vT <- transExpr v
  case arrayT of
    Array aId t -> if nT == Int
                   then do
                     t' <- case t of
                               Record recId Nothing -> lookupTy recId
                               _                    -> return t
                     if vT |> t'
                      then return $ TExpr NoExp (Array aId t')
                      else genError expr $ "v should have type " ++ show t
               else genError expr "n must have type int"
    _       -> genError expr "type of array expr isn't an array type"

transExpr nExpr@(NExpr expr) = do
  TExpr eTrans eType <- transExpr expr
  case eType of
    Int -> do
      tExp <- tNeg eTrans
      return $ TExpr tExp Int
    _   -> genError nExpr "int required"

transExpr expr@(BExpr op l r)
  | op `elem` [Add, Sub, Mult, Div] = do
     TExpr lTrans lType <- transExpr l
     TExpr rTrans rType <- transExpr r
     case (lType, rType) of
       (Int, Int) -> do
        tExp <- tArith op lTrans rTrans
        return $ TExpr tExp Int
       _          -> genError expr "int required"
  | op `elem` [Gt, Lt, GTE, LTE] = do
     TExpr lTrans lType <- transExpr l
     TExpr rTrans rType <- transExpr r
     case (lType, rType) of
       (Int, Int)       -> do
         tExp <- tIntComp op lTrans rTrans
         return $ TExpr tExp Int
       (String, String) -> do
         tExp <- tStringComp op lTrans rTrans
         return $ TExpr tExp Int
       _                -> genError expr "ints or strings required"
  | op `elem` [Equal, NEqual] = do
     TExpr lTrans lType <- transExpr l
     TExpr rTrans rType <- transExpr r
     if lType |> rType || rType |> lType
     then case (lType, rType) of
              (Int, Int) -> do
                     tExp <- tIntComp op lTrans rTrans
                     return $ TExpr tExp Int
              (String, String) -> do
                     tExp <- tStringEqNEq op lTrans rTrans
                     return $ TExpr tExp Int
              _ ->return $ TExpr NoExp Int
     else genError expr $ "ints, strings, array, or recs required"
                                         ++ " left type: " ++ show lType ++ " right type: "
                                         ++ show rType

transExpr expr@(If cond body) = do
    TExpr condTrans condTy <- transExpr cond
    TExpr thenTrans thenTy <- transExpr body
    case condTy of
      Int -> case thenTy of
                   Unit -> do
                     tExp <- tIF condTrans thenTrans
                     return $ TExpr tExp Unit
                   _    -> genError expr "body of if expression must return no value"
      _   -> genError expr "cond of if expression must have type int"

transExpr expr@(IfE cond body1 body2) = do
    TExpr condTrans condTy  <- transExpr cond
    TExpr thenTrans thenTy <- transExpr body1
    TExpr elseTrans elseTy <- transExpr body2
    case condTy of
      Int -> if thenTy == elseTy
             then do
               tExp <- tIFE condTrans thenTrans elseTrans
               return $ TExpr tExp thenTy
             else genError expr "both expressions in IFE must have the same type"
      _        -> genError expr "cond of if expression must have type int"

transExpr expr@(While cond body) = do
    TExpr _ condT <- transExpr cond
    TExpr _ bodyT <- transExprB body
    case condT of
      Int -> case bodyT of
                Unit -> return $ TExpr NoExp Unit
                _    -> genError expr "body of while expression must return no value"
      _   -> genError expr "cond of while expression must have type int"

transExpr expr@(For (Assign (SimpleVar x) min) max body) = do
  TExpr _ minT <- transExpr min
  TExpr _ maxT <- transExpr max
  oldEnv <- S.get
  transDec (VarDec (VarDef x Nothing min))
  TExpr _ bodyT <- transExprB body
  case (minT, maxT) of
    (Int, Int) -> case bodyT of
                     Unit -> return $ TExpr NoExp Unit
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
  fun@FunEntry{..} <- lookupFun f
  transArgsAndTys  <- mapM transExpr args
  let passedArgTys = _tExprTy <$> transArgsAndTys
  let transArgs    = _tExpr   <$> transArgsAndTys
  cLevel           <- getLevel
  tExp             <- fCall fun cLevel transArgs
  if passedArgTys == _funEntryArgTys
  then S.put oldEnv >> return (TExpr tExp _funEntryRetTy)
  else S.put oldEnv >> genError expr "args don't match argtype of function"

transExpr UnitExpr = return $ TExpr NoExp Unit
transExpr expr = genError expr "pattern match failed"

--------------------------------------------------------------------------------
-- Var typechecking
--------------------------------------------------------------------------------

typeCheckVar :: Var -> STEnvT Ty
typeCheckVar var = do
  case var of
    SimpleVar v -> do
      Env{..} <- S.get
      case M.lookup v _envV of
        Just VarEntry{..} -> return _varEntryTy
        Just FunEntry{..} -> genError var "function with same name exists"
        _                 -> genError var "undefined var"

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

transDec :: Dec -> STEnvT ()
transDec d = transDecHeader d >> transDecBody d

transDecHeader :: Dec -> STEnvT ()
transDecHeader (VarDec v) = return ()
transDecHeader (TypeDec tys) =
    if duplicates names
    then genError tys "has duplicate type names"
    else mapM_ addTy tys
    where
      names = map _typeId tys
      addTy t@(Type typeId (DataConst tyId))      = lookupTy tyId >>= \t -> insertTy typeId t
      addTy t@(Type typeId (RecordType tyFields)) = insertTy typeId (Record typeId Nothing)
      addTy t@(Type typeId (ArrayType tyId))      = lookupTy tyId >>= \t -> insertTy typeId (Array typeId t)

transDecHeader (FunDec fs) =
  if duplicates names
  then genError fs "has duplicate function names"
  else mapM_ addF fs
  where names = map _funDefId fs
        addF f@FunDef{..} = do
          argTyPairs <- getFieldTys _funDefArgs
          let argTys =  map snd argTyPairs
          resTy      <- case _funDefType of
                         Nothing       -> return Unit
                         Just resType' -> lookupTy resType'
          let escs = map _fieldEscape _funDefArgs
          insertFun escs _funDefId argTys resTy
          return ()

transDecBody :: Dec -> STEnvT ()
transDecBody (VarDec v@VarDef{..}) =
  case _varDefType of
    Nothing -> do
      TExpr _ t <- transExpr _varDefExpr
      if t == Nil
      then genError v "expressions of type nil must be constrained by a record type"
      else insertVar True _varDefId t
    Just typeId  -> do
      TExpr _ t <- transExpr _varDefExpr
      t'      <- lookupTy typeId
      case t of
        Nil -> case t' of
                 Record _ _ -> insertVar True _varDefId t'
                 _          -> genError v
                               "expressions of type nil must be constrained to records"
        _   -> if t == t'
               then insertVar True _varDefId t'
               else genError v "var type does not match expression type"

transDecBody (TypeDec tys) = mapM_ addTy tys
    where addTy Type{..} =
            case _typeBody of
              RecordType typeFields -> do
                typeFieldTys <- getTypeFieldTys typeFields
                insertTy _typeId $ Record _typeId (Just typeFieldTys)
              _                 -> return ()

transDecBody (FunDec fs) = mapM addF fs >> return ()
  where addF f@FunDef{..} = do
            funEntry@FunEntry{..} <- lookupFun _funDefId
            oldEnv                <- S.get
            let argTyPairs = zipWith (\Field{..} ty -> (_fieldId, ty)) _funDefArgs _funEntryArgTys
            mapM (\(id, ty) -> insertVar True id ty) argTyPairs
            -- Switch level to function's level
            setLevel _funEntryLevel
            TExpr _ tBody     <- transExpr _funDefExpr
            -- Restore level to parent
            resParent funEntry
            if tBody == _funEntryRetTy
            then S.put oldEnv
            else genError f "res type doesn't match the type of the body"

getFieldTys :: [Field] -> STEnvT [(Id, Ty)]
getFieldTys fields = do
  Env{..} <- S.get
  mapM (f _envT) fields
  where f envT Field{..} =
         case M.lookup _fieldType envT of
           Nothing -> genError fields "typefield has invalid fields"
           Just ty -> return (_fieldId, ty)

getTypeFieldTys :: [TypeField] -> STEnvT [(Id, Ty)]
getTypeFieldTys typeFields = do
  Env{..} <- S.get
  mapM (f _envT) typeFields
  where f envT TypeField{..} =
         case M.lookup _typeFieldType envT of
           Nothing -> genError typeFields "typefield has invalid fields"
           Just ty -> return (_typeFieldId, ty)
