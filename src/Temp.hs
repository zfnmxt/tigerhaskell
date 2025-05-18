module Temp
  ( Temp,
    newTemp,
    namedLabel,
    newLabel,
    Label (..),
  )
where

import Symbol

newtype Temp = Temp {unTemp :: Symbol}
  deriving (Show, Eq, Ord)

newTemp :: (MonadSym m) => m Temp
newTemp = Temp <$> newSym "t"

newtype Label = Label {unLabel :: Symbol}
  deriving (Show, Eq, Ord)

namedLabel :: (MonadSym m) => String -> m Label
namedLabel s = Label <$> newSym s

newLabel :: (MonadSym m) => m Label
newLabel = namedLabel "L"
