module Parser.Types where

data Symbol a = N Char | T a deriving (Eq, Ord, Show)

type Rule a = (Symbol a, [Symbol a])

type Grammar a = [Rule a]
