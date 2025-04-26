module ParserTests (tests) where

import AST
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import TigerTests

parserTest :: String -> UntypedExp -> TestTree
parserTest s e =
  testCase s $
    case parse "" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTest_ :: IO FilePath -> TestTree
parserTest_ mfp =
  testCaseInfo "" $ do
    s <- readFile =<< mfp
    case parse "" s of
      Left err -> assertFailure err
      Right e' -> mfp

tests :: TestTree
tests =
  testGroup
    "parser"
    [ testGroup
        "tiger testcases"
        $ map parserTest_ testCases
    ]
