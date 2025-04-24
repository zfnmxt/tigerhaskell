{-# LANGUAGE UndecidableInstances #-}

module Symbol
  ( Symbol (..),
    Tag (..),
    SymTable (..),
    MonadSym (..),
    MonadSymTable (..),
    newSym,
    askSym,
    insertSym,
    initTag,
    nextTag,
  )
where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M

newtype Tag = Tag {getTag :: Int}
  deriving (Show, Eq, Ord)

initTag :: Tag
initTag = Tag 0

nextTag :: Tag -> Tag
nextTag (Tag x) = Tag $ x + 1

data Symbol = Symbol
  { symbolName :: String,
    symbolTag :: Tag
  }
  deriving (Show, Ord)

instance Eq Symbol where
  Symbol _ tag1 == Symbol _ tag2 = tag1 == tag2

class (Monad m) => MonadSym m where
  getSymTag :: m Tag
  putSymTag :: Tag -> m ()

newSym :: (MonadSym m) => String -> m Symbol
newSym s = do
  tag <- getSymTag
  putSymTag $ nextTag tag
  pure $ Symbol s tag

instance (Monad m, MonadState Tag m) => MonadSym m where
  getSymTag = get
  putSymTag = put

newtype SymTable a = SymTable {symTable :: Map Symbol a}
  deriving (Show, Eq, Ord, Semigroup, Monoid)

class (Monad m) => MonadSymTable m a where
  askSymTable :: m (SymTable a)
  withSymTable :: (SymTable a -> SymTable a) -> m b -> m b

askSym :: (MonadSymTable m a) => Symbol -> m (Maybe a)
askSym sym = ((M.!? sym) . symTable) <$> askSymTable

insertSym :: (MonadSymTable m a) => Symbol -> a -> m b -> m b
insertSym sym a = withSymTable (SymTable . M.insert sym a . symTable)
