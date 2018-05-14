module Types where

import AST

data Ty = Int
        | String
        | Record [(Id, Ty)]
        | Array Ty
        | Nil
        | Unit
        | Name Id (Maybe Ty)
