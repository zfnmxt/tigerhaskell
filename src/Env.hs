module Env (EnvEntry (..), prelude) where

import Control.Monad.State
import Data.Map qualified as M
import Symbol
import Types (Ty)
import Types qualified as Ty

data EnvEntry
  = VarEntry Ty
  | FunEntry [Ty] Ty
  deriving (Eq, Show, Ord)

prelude :: (SymTable EnvEntry, SymTable Ty)
prelude = flip evalState initTag $ (,) <$> funsM <*> typesM
  where
    mkTable :: [(String, a)] -> State Tag (SymTable a)
    mkTable m = SymTable . M.fromList <$> mapM (\(s, ty) -> (,ty) <$> newSym s) m

    funsM :: State Tag (SymTable EnvEntry)
    funsM =
      mkTable
        [ ("print", FunEntry [Ty.String] Ty.Unit),
          ("flush", FunEntry [] Ty.Unit),
          ("getchar", FunEntry [] Ty.String),
          ("ord", FunEntry [Ty.String] Ty.Int),
          ("chr", FunEntry [Ty.Int] Ty.String),
          ("size", FunEntry [Ty.String] Ty.Int),
          ("substring", FunEntry [Ty.String, Ty.Int, Ty.Int] Ty.String),
          ("concat", FunEntry [Ty.String, Ty.String] Ty.String),
          ("not", FunEntry [Ty.Int] Ty.Int),
          ("exit", FunEntry [Ty.Int] Ty.Unit)
        ]
    typesM :: State Tag (SymTable Ty)
    typesM =
      mkTable
        [ ("int", Ty.Int),
          ("string", Ty.String)
        ]
