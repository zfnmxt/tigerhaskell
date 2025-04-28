module Env (EnvEntry (..), prelude, preludeTag) where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Symbol
import Types (Ty)
import Types qualified as Ty

data EnvEntry
  = VarEntry Ty
  | FunEntry [Ty] Ty
  deriving (Eq, Show, Ord)

prelude :: ((Map String Symbol, SymTable EnvEntry), (Map String Symbol, SymTable Ty))
prelude = fst prelude'

preludeTag :: Tag
preludeTag = snd prelude'

prelude' :: (((Map String Symbol, SymTable EnvEntry), (Map String Symbol, SymTable Ty)), Tag)
prelude' = flip runState initTag $ (,) <$> funsM <*> typesM
  where
    mkTable :: [(String, a)] -> State Tag (Map String Symbol, SymTable a)
    mkTable m = do
      (sym_map, table_map) <-
        unzip
          <$> mapM
            ( \(s, a) -> do
                sym <- newSym s
                pure ((s, sym), (sym, a))
            )
            m
      pure (M.fromList sym_map, SymTable $ M.fromList table_map)

    funsM :: State Tag (Map String Symbol, SymTable EnvEntry)
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
    typesM :: State Tag (Map String Symbol, SymTable Ty)
    typesM =
      mkTable
        [ ("int", Ty.Int),
          ("string", Ty.String)
        ]
