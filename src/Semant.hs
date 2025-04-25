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

transProg :: UntypedExp -> TransM (Exp ::: Ty)
transProg = undefined

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

transTy :: UntypedTy -> TransM (AST.Ty Symbol)
transTy = undefined

transDec :: UntypedDec -> TransM Dec
transDec (VarDec s mty e pos) = do
  e' ::: e_ty <- transExp e
  mtyt <- case mty of
    Nothing -> pure Nothing
    Just (ty_s, ty_pos) -> do
      ty_sym <- lookupSym' ty_s ty_pos
      ty <- lookupTy' ty_sym ty_pos
      unless (validConstraint ty e_ty) $
        throwError $
          InvalidType e_ty (S.singleton ty) ty_pos
      pure $ Just (ty_sym, ty_pos)
  sym <- newSym s
  pure $ VarDec sym mtyt e' pos
  where
    validConstraint (Record _ _) Nil = True
    validConstraint ty e_ty = ty == e_ty
transDec (TypeDec s ty pos) =
  TypeDec <$> newSym s <*> transTy ty <*> pure pos
