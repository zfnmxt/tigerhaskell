module Semant (transProg) where

import AST hiding (Dec, Exp, Field, FunDec, Ty, Var)
import AST qualified as AST
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Env qualified
import Frame (Frame)
import Symbol
import Temp qualified
import Translate
import TypeCheck
import Types

data Error
  = Error String (Maybe SourcePos)
  deriving (Show, Eq)

data TransEntry frame
  = VarEntry (Access frame) Ty
  | FunEntry (Level frame) Temp.Label [Ty] Ty

data Env frame = Env
  { envVal :: SymTable (TransEntry frame),
    envLevel :: Level frame
  }

newtype TransM frame a = TransM {runTransM :: ExceptT Error (RWS (Env frame) () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env frame),
      MonadWriter (),
      MonadState Tag,
      MonadError Error
    )

transProg = undefined

--
-- transExp :: (Frame frame) => Exp -> TransM frame a
-- transExp = undefined
--
-- transDec :: (Frame frame) => Dec -> TransM frame a -> TransM frame a
-- transDec (FunctionDec decs) m = transFunDec decs
--  where
--    transFunDec (AST.FunDec f params mrt body pos : ds) = do
--      lvl <- asks envLevel
--      let lvl' = newLevel lvl (Temp.Label f) (map (const True) params)
--      insertSym f (FunEntry lvl') (Temp.Label f)
--      local (\env -> env {
