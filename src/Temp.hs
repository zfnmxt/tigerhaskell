module Temp (Label (..)) where

import Symbol

newtype Label = Label Symbol
  deriving (Show, Eq)
