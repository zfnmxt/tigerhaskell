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
  | Name Symbol (Maybe Ty)
  | Unit
  deriving (Show, Eq, Ord)

isRecord :: Ty -> Bool
isRecord (Record {}) = True
isRecord Nil = True
isRecord (Name s (Just t)) = isRecord t
isRecord _ = False

fieldType :: Symbol -> Ty -> Maybe Ty
fieldType field (Record fields _) = lookup field fields
fieldType field (Name _ (Just t)) = fieldType field t
fieldType _ _ = Nothing

recordFields :: Ty -> Maybe [(Symbol, Ty)]
recordFields (Record fields _) = pure fields
recordFields Nil = pure mempty
recordFields (Name s (Just t)) = recordFields t
recordFields _ = mempty

elemType :: Ty -> Maybe Ty
elemType (Array t _) = Just t
elemType (Name _ (Just t)) = elemType t
elemType _ = Nothing

isNameType :: Ty -> Bool
isNameType Name {} = True
isNameType _ = False
