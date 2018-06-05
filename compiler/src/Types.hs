module Types where

import AST

type Unique = Int
data Ty = Int
        | String
        | Record [(Id, Ty)] Unique
        | Array Ty Unique
        | Nil
        | Unit
        | Rec TypeId
        deriving (Show, Eq)


(|>) :: Ty -> Ty -> Bool
Nil |> Record _ _      = True
Record xs ux |> Record ys uy = Record xs ux == Record ys uy
Int |> Int             = True
String |> String       = True
Array x ux |> Array y uy    = Array x ux == Array y uy
Nil |> Nil             = True
Unit |> Unit           = True
_ |> _                 = False

