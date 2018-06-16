module Temp where

import Frame

data Temp      = Temp Int  deriving (Eq, Show)
               | RTemp Reg
data Label     = Label Int deriving (Eq, Show)
