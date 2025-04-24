module Types (Ty (..)) where

import Symbol
import Prelude hiding (Int, String)

data Ty
  = Record [(Symbol, Ty)] Tag
  | Nil
  | Int
  | String
  | Array Ty Tag
  | Name Symbol (Maybe Ty)
  | Unit
  deriving (Show, Eq, Ord)
