module Types
  ( Ty (..),
    isRecord,
    fieldType,
    recordFields,
    elemType,
    isNameType,
  )
where

import AST qualified
import Symbol
import Prelude hiding (Int, String)

data Ty
  = Record [(Symbol, Ty)] Tag
  | Nil
  | Int
  | String
  | Array Ty Tag
  | Name Symbol
  | Unit
  deriving (Show, Eq, Ord)

isRecord :: Ty -> Bool
isRecord (Record {}) = True
isRecord Nil = True
isRecord _ = False

fieldType :: Symbol -> Ty -> Maybe Ty
fieldType field (Record fields _) = lookup field fields
fieldType _ _ = Nothing

recordFields :: Ty -> Maybe [(Symbol, Ty)]
recordFields (Record fields _) = pure fields
recordFields Nil = pure mempty
recordFields _ = mempty

elemType :: Ty -> Maybe Ty
elemType (Array t _) = Just t
elemType _ = Nothing

isNameType :: Ty -> Bool
isNameType Name {} = True
isNameType _ = False
