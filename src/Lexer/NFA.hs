module Lexer.NFA where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lexer.Finite as F
import Lexer.Finite ((:->) (..))

data (Ord a, Ord s) => NFA a s
  = DFA
      { delta :: (a, s) :-> s,
        start :: s,
        accept :: S.Set s,
        states :: S.Set s,
        alphabet :: S.Set a
      }
