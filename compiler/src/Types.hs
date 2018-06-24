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
Record xs ux |> Record ys uy = xs == ys
Int |> Int             = True
String |> String       = True
Array x ux |> Array y uy    = x == y 
Nil |> Nil             = True
Unit |> Unit           = True
_ |> _                 = False


(<|>) :: Ty -> Ty -> Bool
t1 <|> t2 = t1 |> t2 || t2 |> t1
