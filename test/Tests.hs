module Main (main) where

import ParserTests qualified
import SemantTests qualified
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ ParserTests.tests,
      SemantTests.tests
    ]

main :: IO ()
main = defaultMain tests
