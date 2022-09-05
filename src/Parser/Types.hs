module Parser.Types where

import Data.Map (Map)

data Symbol a = T a | N a deriving (Eq, Ord, Show)

type Rule a = (Symbol a, [Symbol a])

type Grammar a = [Rule a]
