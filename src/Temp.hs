module Temp
  ( Temp,
    newTemp,
    Label (..),
  )
where

import Symbol

newtype Temp = Temp {unTemp :: Symbol}
  deriving (Show, Eq)

newTemp :: (MonadSym m) => m Temp
newTemp = Temp <$> newSym "t"

newtype Label = Label {unLabel :: Symbol}
  deriving (Show, Eq)

namedLabel :: (MonadSym m) => String -> m Label
namedLabel s = Label <$> newSym s

newLabel :: (MonadSym m) => m Label
newLabel = namedLabel "L"
