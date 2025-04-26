module Main (main) where

import ParserTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ParserTests.tests]

main :: IO ()
main = defaultMain tests
