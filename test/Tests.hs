module Main (main) where

import ParserTests qualified
import Test.Tasty
import TypeCheckTests qualified

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ ParserTests.tests,
      TypeCheckTests.tests
    ]

main :: IO ()
main = defaultMain tests
