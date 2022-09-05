module Util where

converge :: Eq a => a -> (a -> a) -> a
converge a f
  | f a == a = a
  | otherwise = converge (f a) f

for :: [a] -> (a -> b) -> [b]
for = flip map
