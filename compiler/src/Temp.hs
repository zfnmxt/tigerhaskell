module Temp where

import Registers

data Temp      = Temp Int 
               | RTemp Reg
               deriving (Eq, Show)
data Label     = Label Int deriving (Eq, Show)
