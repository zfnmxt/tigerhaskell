module Types where

import AST

data Ty = Int
        | String
        | Record [(Id, Ty)]
        | Array Ty
        | Nil
        | Unit
        deriving (Show, Eq)


(:>) :: Ty -> Ty -> Bool
Nil :> Record _      = True
Record x :> Record y = x :> y
Int :> Int           = True
String :> Stirng     = True
Array x :> Array y   = x :> y
Nil :> Nil           = True
Unit :> Unit         = True

