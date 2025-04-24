module Semant (transProg) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad.RWS
import Symbol
import Types

type Var = AST.Var Symbol

type Exp = AST.Exp Symbol

type ASTTy = AST.Ty Symbol

type Dec = AST.Dec Symbol

type FunDec = AST.FunDec Symbol

type Field = AST.Field Symbol

data EnvEntry
  = VarEntry Ty
  | FunEntry [Ty] Ty
  deriving (Eq, Show, Ord)

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

transProg :: UntypedExp -> TransM Exp
transProg = undefined

transVar :: UntypedVar -> TransM Var
transVar = undefined

transExp :: UntypedExp -> TransM Exp
transExp = undefined

transTy :: UntypedTy -> TransM Ty
transTy = undefined
