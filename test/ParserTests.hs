module ParserTests (tests) where

import AST
import Control.Monad
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import qualified TigerTests

parserTest :: String -> UntypedExp -> TestTree
parserTest s e =
  testCase s $
    case parse "" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

tests :: TestTree
tests =
  testGroup
    "parser"
    [ testCaseSteps "tiger testcases" $ \step -> do
        tests <- TigerTests.testCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right {} -> pure ()
    ]
