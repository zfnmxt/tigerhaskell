module SemantTests (tests) where

import AST
import Control.Monad
import Parser
import Semant
import Test.Tasty
import Test.Tasty.HUnit
import qualified TigerTests

tests :: TestTree
tests =
  testGroup
    "semant"
    [ testCaseSteps "tiger testcases" $ \step -> do
        tests <- TigerTests.testCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right e ->
              case transProg e of
                Left err' -> assertFailure $ show err'
                Right {} -> pure ()
    ]
