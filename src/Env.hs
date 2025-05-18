{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Env (EnvEntry (..), prelude, preludeTag) where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Frame (Frame, X86)
import Symbol
import Temp qualified
import Translate
import Types (Ty)
import Types qualified as Ty

data EnvEntry frame
  = VarEntry (Access frame) Ty
  | FunEntry (Level frame) Temp.Label [Ty] Ty

deriving instance (Show frame, Show (Access frame)) => Show (EnvEntry frame)

deriving instance (Eq frame, Eq (Access frame)) => Eq (EnvEntry frame)

deriving instance (Ord frame, Ord (Access frame)) => Ord (EnvEntry frame)

prelude ::
  (Frame frame) =>
  ((Map String Symbol, SymTable (EnvEntry frame)), (Map String Symbol, SymTable Ty))
prelude = fst prelude'

preludeTag :: Tag
preludeTag = snd (prelude' @X86)

prelude' ::
  (Frame frame) =>
  (((Map String Symbol, SymTable (EnvEntry frame)), (Map String Symbol, SymTable Ty)), Tag)
prelude' = flip runState initTag $ (,) <$> funsM <*> typesM
  where
    mkTable :: [(String, Symbol -> a)] -> State Tag (Map String Symbol, SymTable a)
    mkTable m = do
      (sym_map, table_map) <-
        unzip
          <$> mapM
            ( \(s, a) -> do
                sym <- newSym s
                pure ((s, sym), (sym, a sym))
            )
            m
      pure (M.fromList sym_map, SymTable $ M.fromList table_map)

    funsM :: (Frame frame) => State Tag (Map String Symbol, SymTable (EnvEntry frame))
    funsM =
      mkTable
        [ ("print", \sym -> FunEntry outermost (Temp.Label sym) [Ty.String] Ty.Unit),
          ("flush", \sym -> FunEntry outermost (Temp.Label sym) [] Ty.Unit),
          ("getchar", \sym -> FunEntry outermost (Temp.Label sym) [] Ty.String),
          ("ord", \sym -> FunEntry outermost (Temp.Label sym) [Ty.String] Ty.Int),
          ("chr", \sym -> FunEntry outermost (Temp.Label sym) [Ty.Int] Ty.String),
          ("size", \sym -> FunEntry outermost (Temp.Label sym) [Ty.String] Ty.Int),
          ("substring", \sym -> FunEntry outermost (Temp.Label sym) [Ty.String, Ty.Int, Ty.Int] Ty.String),
          ("concat", \sym -> FunEntry outermost (Temp.Label sym) [Ty.String, Ty.String] Ty.String),
          ("not", \sym -> FunEntry outermost (Temp.Label sym) [Ty.Int] Ty.Int),
          ("exit", \sym -> FunEntry outermost (Temp.Label sym) [Ty.Int] Ty.Unit)
        ]
    typesM :: State Tag (Map String Symbol, SymTable Ty)
    typesM =
      mkTable
        [ ("int", const Ty.Int),
          ("string", const Ty.String)
        ]
