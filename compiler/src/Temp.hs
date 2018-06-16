module Temp where

import Registers

data Temp      = Temp Int  deriving (Eq, Show)
               | RTemp Reg
data Label     = Label Int deriving (Eq, Show)
