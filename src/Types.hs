module Types
  ( Ty (..),
    isRecord,
    unpack,
    fieldType,
    recordFields,
    elemType,
    isNameType,
  )
where

import AST qualified
import Control.Monad.Trans.Maybe
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

unpack :: (MonadSymTable m Ty) => Ty -> m (Maybe Ty)
unpack (Name sym) = do
  mty <- askSym sym
  case mty of
    Just (Name sym') -> unpack (Name sym')
    Just t -> pure $ Just t
    Nothing -> pure Nothing
unpack t = pure $ Just t

fieldType :: (MonadSymTable m Ty) => Symbol -> Ty -> m (Maybe Ty)
fieldType field (Record fields _) = do
  let mty = lookup field fields
  case mty of
    Nothing -> pure Nothing
    Just ty -> unpack ty
fieldType _ _ = pure Nothing

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
