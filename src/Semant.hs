module Semant (transProg) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Env
import Symbol
import Translate qualified
import Types

type Var = AST.Var Symbol

type Exp = AST.Exp Symbol

type ASTTy = AST.Ty Symbol

type Dec = AST.Dec Symbol

type FunDec = AST.FunDec Symbol

type Field = AST.Field Symbol

data Env = Env
  { envVal :: SymTable EnvEntry,
    envTy :: SymTable Ty,
    envSym :: Map String Symbol
  }
  deriving (Eq, Show, Ord)

instance Semigroup Env where
  Env vEnv1 tEnv1 sEnv1 <> Env vEnv2 tEnv2 sEnv2 =
    Env (vEnv1 <> vEnv2) (tEnv1 <> tEnv2) (sEnv1 <> sEnv2)

instance Monoid Env where
  mempty = Env mempty mempty mempty

initEnv :: Env
initEnv =
  Env
    { envVal = prelude_val,
      envTy = prelude_ty,
      envSym = mempty
    }
  where
    (prelude_val, prelude_ty) = prelude

data Error
  = InvalidType Ty (Set Ty) SourcePos
  | UndefinedVar String SourcePos
  | UndefinedType String SourcePos
  deriving (Show, Eq)

newtype TransM a = TransM {runTransM :: RWST Env () Tag (Either Error) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

instance MonadSymTable TransM EnvEntry where
  askSymTable = asks envVal
  withSymTable f = local $ \env -> env {envVal = f $ envVal env}

instance MonadSymTable TransM Ty where
  askSymTable = asks envTy
  withSymTable f = local $ \env -> env {envTy = f $ envTy env}

data (:::) a b = a ::: b

typeOf :: (a ::: b) -> b
typeOf (_ ::: b) = b

deannotate :: (a ::: b) -> a
deannotate (a ::: _) = a

lookupSym :: String -> TransM (Maybe Symbol)
lookupSym s = (M.!? s) <$> asks envSym

lookupSym' :: String -> SourcePos -> TransM Symbol
lookupSym' s pos = do
  msym <- lookupSym s
  case msym of
    Nothing -> throwError $ UndefinedVar s pos
    Just sym -> pure sym

lookupVar :: Symbol -> TransM (Maybe Ty)
lookupVar = askSym

lookupVar' :: Symbol -> SourcePos -> TransM Ty
lookupVar' sym pos = do
  mentry <- askSym sym
  case mentry of
    Just (VarEntry ty) -> pure ty
    _ -> throwError $ UndefinedVar (symName sym) pos

lookupTy :: Symbol -> TransM (Maybe Ty)
lookupTy = askSym

lookupTy' :: Symbol -> SourcePos -> TransM Ty
lookupTy' sym pos = do
  mty <- lookupTy sym
  case mty of
    Nothing -> throwError $ UndefinedType (symName sym) pos
    Just ty -> pure ty

withSym :: String -> (Symbol -> TransM a) -> TransM a
withSym s m = do
  mname <- (M.!? s) <$> asks envSym
  case mname of
    Just sym -> m sym
    Nothing -> do
      sym <- newSym s
      local (\env -> env {envSym = M.insert s sym $ envSym env}) $ m sym

transProg :: UntypedExp -> Either Error (Exp ::: Ty)
transProg e = fst <$> evalRWST (runTransM $ transExp e) initEnv initTag

transVar :: UntypedVar -> TransM (Var ::: Ty)
transVar (SimpleVar s pos) = do
  v <- lookupSym' s pos
  ty <- lookupVar' v pos
  pure $ SimpleVar v pos ::: ty

check_ :: (a ::: Ty) -> [Ty] -> SourcePos -> TransM ()
check_ a tys pos = void $ check a tys pos

check :: (a ::: Ty) -> [Ty] -> SourcePos -> TransM Ty
check a tys pos
  | typeOf a `elem` tys = pure $ typeOf a
  | otherwise = throwError $ InvalidType (typeOf a) (S.fromList tys) pos

transExp :: UntypedExp -> TransM (Exp ::: Ty)
transExp (VarExp v) = do
  v' ::: v_ty <- transVar v
  pure $ VarExp v' ::: v_ty
transExp (OpExp l op r pos) = do
  lt@(l' ::: l_ty) <- transExp l
  rt@(r' ::: r_ty) <- transExp r
  case op of
    _
      | op `elem` [PlusOp, MinusOp, TimesOp, DivideOp] -> do
          check_ lt [Int] pos
          check_ rt [Int] pos
          pure $ OpExp l' op r' pos ::: Int
      | otherwise -> do
          check_ lt [Int, String] pos
          check_ rt [Int, String] pos
          when (l_ty /= r_ty) $
            throwError $
              InvalidType r_ty (S.singleton l_ty) pos
          pure $ OpExp l' op r' pos ::: Int

transField :: UntypedField -> TransM (Field ::: Ty)
transField (AST.Field field ty_s pos) = do
  field_sym <- newSym field
  ty_sym <- lookupSym' ty_s pos
  ty <- lookupTy' ty_sym pos
  pure $ AST.Field field_sym ty_sym pos ::: ty

transTy :: UntypedTy -> TransM (AST.Ty Symbol ::: Ty)
transTy (NameTy s pos) = do
  sym <- lookupSym' s pos
  ty <- lookupTy' sym pos
  pure $ NameTy sym pos ::: ty
transTy (RecordTy fields) = do
  fields' <- mapM transField fields
  tag <- newTag
  let ty_fields = map (\(AST.Field sym _ _ ::: ty) -> (sym, ty)) fields'
      ty = Record ty_fields tag
  pure $ RecordTy (map deannotate fields') ::: ty
transTy (ArrayTy s pos) = do
  sym <- lookupSym' s pos
  ty <- lookupTy' sym pos
  pure $ ArrayTy sym pos ::: ty

transDec :: UntypedDec -> (Dec -> TransM a) -> TransM a
transDec (FunctionDec (AST.FunDec f params mrt body pos)) next = do
  params' <- mapM transField params
  body' ::: body_ty <-
    withParams params' $ transExp body
  mrt' <-
    case mrt of
      Nothing -> pure Nothing
      Just (rt_s, pos) -> do
        rt_sym <- lookupSym' rt_s pos
        rt <- lookupTy' rt_sym pos
        checkTypeAnnot pos body_ty rt
        pure $ Just (rt_sym, pos)
  f' <- newSym f
  insertSym f' (FunEntry (map typeOf params') body_ty) $
    next (FunctionDec $ AST.FunDec f' (map deannotate params') mrt' body' pos)
  where
    withParams :: [Field ::: Ty] -> TransM a -> TransM a
    withParams [] m = m
    withParams (AST.Field field _ _ ::: ty : ps) m =
      insertSym field (VarEntry ty) $ withParams ps m
transDec (VarDec s mty e pos) m = do
  e' ::: e_ty <- transExp e
  mtyt <- case mty of
    Nothing -> pure Nothing
    Just (ty_s, ty_pos) -> do
      ty_sym <- lookupSym' ty_s ty_pos
      ty <- lookupTy' ty_sym ty_pos
      checkTypeAnnot ty_pos e_ty ty
      pure $ Just (ty_sym, ty_pos)
  sym <- newSym s
  insertSym
    sym
    (VarEntry e_ty)
    (m $ VarDec sym mtyt e' pos)
transDec (TypeDec s sty pos) m = do
  sym <- newSym s
  sty' ::: ty <- transTy sty
  insertSym sym ty (m $ TypeDec sym sty' pos)

validTypeAnnot :: Ty -> Ty -> Bool
validTypeAnnot Nil (Record _ _) = True
validTypeAnnot e_ty ty = ty == e_ty

checkTypeAnnot :: SourcePos -> Ty -> Ty -> TransM ()
checkTypeAnnot pos e_ty ty =
  unless (e_ty `validTypeAnnot` ty) $
    throwError $
      InvalidType e_ty (S.singleton ty) pos
