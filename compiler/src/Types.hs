module Types where

import AST

data Ty = Int
        | String
        | Record TypeId (Maybe [(Id, Ty)])
        | Array  TypeId Ty
        | Nil
        | Unit
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

