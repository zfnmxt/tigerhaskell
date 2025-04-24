module Semant (transProg) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad.RWS
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
    envTy :: SymTable Ty
  }
  deriving (Eq, Show, Ord)

instance Semigroup Env where
  Env vEnv1 tEnv1 <> Env vEnv2 tEnv2 =
    Env (vEnv1 <> vEnv2) (tEnv1 <> tEnv2)

instance Monoid Env where
  mempty = Env mempty mempty

newtype TransM a = TransM {runTransM :: RWS Env () Tag a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (),
      MonadSym
    )

instance MonadSymTable TransM EnvEntry where
  askSymTable = asks envVal
  withSymTable f = local $ \env -> env {envVal = f $ envVal env}

instance MonadSymTable TransM Ty where
  askSymTable = asks envTy
  withSymTable f = local $ \env -> env {envTy = f $ envTy env}

data ExpTy = ExpTy Translate.Exp Ty

typeOf :: ExpTy -> Ty
typeOf (ExpTy _ ty) = ty

transProg :: UntypedExp -> TransM ExpTy
transProg = undefined

transVar :: UntypedVar -> TransM ExpTy
transVar = undefined

transExp :: UntypedExp -> TransM ExpTy
transExp (OpExp l op r _) = do
 l' <- transExp l
 r' <- transExp r
 if op `elem` [PlusOp, MinusOp, TimesOp, DivideOp]
   then


transTy :: UntypedTy -> TransM Ty
transTy = undefined
