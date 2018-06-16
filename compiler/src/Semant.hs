{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Semant where

import AST
import Types
import Frame as F
import Translate as T
import Temp (Temp (..), Label (..))
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Set (Set)
import Data.List (replicate)
import qualified Data.Set as S
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.State.Lazy (lift)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, execStateT)
import qualified Data.List as L


data VEnvEntry = VarEntry {_varEntryTy :: Ty, _varEntryAccess  :: T.Access}
               | FunEntry { _funEntryArgTys  :: [Ty]
                          , _funEntryRetTy   :: Ty
                          , _funEntryLevel   :: T.Level
                          , _funEntryLabel   :: Label
                          }
               deriving (Show, Eq)

type TExpr = ()
type TExprTy = (TExpr, Ty)
type EnvV = Map Id VEnvEntry
type EnvT = Map TypeId Ty
data Env  = Env { _envV      :: EnvV
                , _envT      :: EnvT
                , _envTemp   :: Int
                , _envLabel  :: Int
                , _envLevel  :: T.Level
                , _envLevels :: [T.Level]
                }

type TError        = String
type CheckerState  = StateT Env (Either TError)

_PREDEFINED_FUNCS = 10

genError :: Show a => a -> String -> CheckerState b
genError x s = lift . Left $ "Error on term: " ++ show x ++ " with error msg: " ++ s

--------------------------------------------------------------------------------
-- Initial env
--------------------------------------------------------------------------------
baseTEnv = M.fromList [("int", Int), ("string", String)]
baseVEnv = M.fromList [ ("print",     FunEntry [String] Unit Outermost (Label 0)            )
                      , ("flush",     FunEntry [] Unit Outermost (Label 1)                  )
                      , ("getchar",   FunEntry [] String Outermost (Label 2)                )
                      , ("ord",       FunEntry [String] Int Outermost (Label 3)             )
                      , ("chr",       FunEntry [Int] String Outermost (Label 4)             )
                      , ("size",      FunEntry [String] Int Outermost (Label 5)             )
                      , ("substring", FunEntry [String, Int, Int] String Outermost (Label 6))
                      , ("concat",    FunEntry [String, String] String Outermost (Label 7)  )
                      , ("not",       FunEntry [Int] Int Outermost (Label 8)                )
                      , ("exit",      FunEntry [Int] Unit Outermost (Label 9)               )
                      ]

initEnv = Env { _envV       = baseVEnv
              , _envT       = baseTEnv
              , _envTemp    = 1
              , _envLabel   = _PREDEFINED_FUNCS
              , _envLevels  = [T.Level T.Outermost (newFrame (Label 0) [])]
              , _envLevel   = T.Level T.Outermost (newFrame (Label 0) [])
              }

--------------------------------------------------------------------------------
-- Env functions
--------------------------------------------------------------------------------

insertVar :: Bool -> Id -> Ty -> CheckerState ()
insertVar esc id ty = do
  env@Env{..} <- S.get
  temp        <- mkTemp
  let (level', access) = T.allocLocal _envLevel temp esc
  S.put $ env { _envV     = M.insert id (VarEntry ty access) _envV
              , _envLevel = level'
              }

insertFun :: [Bool] -> Id -> [Ty] -> Ty -> CheckerState ()
insertFun escs id argTys resTy = do
  env@Env{..} <- S.get
  levelLabel  <- mkLabel
  funLabel    <- mkLabel
  let level  = newLevel _envLevel levelLabel escs
  S.put $ env { _envV      = M.insert id (FunEntry argTys resTy level funLabel) _envV
              , _envLevel  = level
              , _envLevels = [level] ++ _envLevels
              }

lookupVar :: Id -> CheckerState VEnvEntry
lookupVar id = do
  Env{..} <- S.get
  case M.lookup id _envV of
    Just var@VarEntry{..} -> return var
    Just FunEntry{..}     -> genError id "found function instead of var"
    Nothing               -> genError id "var not found"

lookupFun :: Id -> CheckerState VEnvEntry
lookupFun id = do
  Env{..} <- S.get
  case M.lookup id _envV of
    Just VarEntry{..}     -> genError id "found var instead of function"
    Just fun@FunEntry{..} -> return fun
    Nothing               -> genError id "var not found"

insertTy :: TypeId -> Ty -> CheckerState ()
insertTy typeId ty = do
  env@Env{..} <- S.get
  S.put $ env { _envT =  M.insert typeId ty _envT }

lookupTy :: TypeId -> CheckerState Ty
lookupTy tId = do
  Env{..} <- S.get
  case M.lookup tId _envT of
    Just t  -> return t
    Nothing -> genError tId "type not found"

mkTemp :: CheckerState Temp
mkTemp = do
   env@Env{..} <- S.get
   S.put  $ env { _envTemp = _envTemp + 1}
   return $ Temp _envTemp

mkLabel :: CheckerState Label
mkLabel = do
   env@Env{..} <- S.get
   S.put  $ env { _envLabel = _envLabel + 1}
   return $ Label _envLabel

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
               let f (RecordField{..}, (fname, ftype)) = do
                     (_, fexprT) <- transExpr _recordFieldExpr
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
  fun@FunEntry{..} <- lookupFun f
  passedArgTys <- mapM (\a -> snd <$> transExpr a) args
  if passedArgTys == _funEntryArgTys
  then S.put oldEnv >> return ((), _funEntryRetTy)
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
      Env{..} <- S.get
      case M.lookup v _envV of
        Just VarEntry{..} -> return _varEntryTy
        Just FunEntry{..} -> genError var "function with same name exists"
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
        addF f@(FunDef funId args resType body) = do
          argTyPairs <- getFieldTys args
          let argTys =  map snd argTyPairs
          resTy      <- case resType of
                         Nothing       -> return Unit
                         Just resType' -> lookupTy resType'
          let escs = replicate (length args) True
          insertFun escs funId argTys resTy
          return ()

--insertVar :: Bool -> Id -> Ty -> CheckerState ()
--insertVar esc id ty = do
--
--insertFun :: [Bool] -> Id -> [Ty] -> Ty -> CheckerState ()

transDecBody :: Dec -> CheckerState ()
transDecBody (VarDec v@VarDef{..}) =
  case _varDefType of
    Nothing -> do
      (_, t) <- transExpr _varDefExpr
      if t == Nil
      then genError v "expressions of type nil must be constrained by a record type"
      else insertVar True _varDefId t
    Just typeId  -> do
      (_, t)  <- transExpr _varDefExpr
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
            FunEntry{..}   <- lookupFun _funDefId
            oldEnv         <- S.get
            let argTyPairs = zipWith (\Field{..} ty -> (_fieldId, ty)) _funDefArgs _funEntryArgTys
            mapM (\(id, ty) -> insertVar True id ty) argTyPairs
            (_, tBody)     <- transExpr _funDefExpr
            if tBody == _funEntryRetTy
            then S.put oldEnv
            else genError f "res type doesn't match the type of the body"

getFieldTys :: [Field] -> CheckerState [(Id, Ty)]
getFieldTys fields = do
  Env{..} <- S.get
  mapM (f _envT) fields
  where f envT Field{..} =
         case M.lookup _fieldType envT of
           Nothing -> genError fields "typefield has invalid fields"
           Just ty -> return (_fieldId, ty)

getTypeFieldTys :: [TypeField] -> CheckerState [(Id, Ty)]
getTypeFieldTys typeFields = do
  Env{..} <- S.get
  mapM (f _envT) typeFields
  where f envT TypeField{..} =
         case M.lookup _typeFieldType envT of
           Nothing -> genError typeFields "typefield has invalid fields"
           Just ty -> return (_typeFieldId, ty)
