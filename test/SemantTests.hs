module SemantTests (tests) where

import AST
import Control.Monad
import Parser
import Semant
import Test.Tasty
import Test.Tasty.HUnit
import qualified TigerTests
import qualified Types

semantTest' :: String -> Maybe Types.Ty -> TestTree
semantTest' s mt = semantTest s s mt

semantTest :: String -> String -> Maybe Types.Ty -> TestTree
semantTest n s mt =
  testCase n $
    case parse "" s of
      Left err -> assertFailure err
      Right e ->
        case transProg e of
          Left err' -> assertFailure $ show err'
          Right (_ ::: ty) ->
            case mt of
              Nothing -> pure ()
              Just ty' -> ty' @?= ty

tests :: TestTree
tests =
  testGroup
    "semant"
    [ testGroup
        "simple"
        [ semantTest' "1" $ Just Types.Int,
          semantTest' "1 + 2" $ Just Types.Int,
          semantTest' "let var x := 5 in x end" $ Just Types.Int,
          semantTest'
            ( unwords
                [ "let var x := 1",
                  "var x := 2",
                  "in x end"
                ]
            )
            $ Just Types.Int,
          semantTest'
            ( unwords
                [ "let var x := 1",
                  "var x := \"hello\"",
                  "in x end"
                ]
            )
            $ Just Types.String,
          semantTest' "let type t = int in 0 end" $ Just Types.Int
        ],
      testCaseSteps "tiger valid testcases" $ \step -> do
        tests <- TigerTests.validTestCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right e ->
              case transProg e of
                Left err' -> assertFailure $ show err'
                Right {} -> pure (),
      testCaseSteps "tiger error testcases" $ \step -> do
        tests <- TigerTests.errorTestCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right e ->
              case transProg e of
                Left err' -> pure ()
                Right {} -> assertFailure "expected failure"
    ]
