module Regex where

data Regex a
  = Sym a
  | (:||) (Regex a) (Regex a)
  | (:::) (Regex a) (Regex a)
  | Empty
  | Star (Regex a)
  deriving (Show, Eq)

oneOf :: [a] -> Regex a
oneOf = foldr (\a b -> b :|| Sym a) Empty

maybe :: Regex a -> Regex a
maybe r = r :|| Empty

plus :: Regex a -> Regex a
plus r = r ::: Star r
