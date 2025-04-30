module Semant (transProg) where

import Env qualified
import TypeCheck

data Error
  = Error String (Maybe SourcePos)
  deriving (Show, Eq)

data TransEntry
  = VarEntry

data Env = Env
  { envVal :: SymTable TransEntry
  }
  deriving (Eq, Show, Ord)

newtype TransM a = TransM {runTransM :: ExceptT Error (RWS Env () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

transProg = undefined

transExp :: Exp -> TransM a
transExp = undefined

transDec :: Dec -> TransM a -> TransM a
transDec = undefined
