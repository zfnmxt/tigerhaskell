module TypeCheckTests (tests) where

import AST
import Control.Monad
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import qualified TigerTests
import TypeCheck
import qualified Types

typeCheckTest' :: String -> Maybe Types.Ty -> TestTree
typeCheckTest' s mt = typeCheckTest s s mt

typeCheckTest :: String -> String -> Maybe Types.Ty -> TestTree
typeCheckTest n s mt =
  testCase n $
    case parse "" s of
      Left err -> assertFailure err
      Right e ->
        case checkProg e of
          Left err' -> assertFailure $ show err'
          Right (_ ::: ty) ->
            case mt of
              Nothing -> pure ()
              Just ty' -> ty' @?= ty

tests :: TestTree
tests =
  testGroup
    "typeCheck"
    [ testGroup
        "simple"
        [ typeCheckTest' "1" $ Just Types.Int,
          typeCheckTest' "1 + 2" $ Just Types.Int,
          typeCheckTest' "let var x := 5 in x end" $ Just Types.Int,
          typeCheckTest'
            ( unwords
                [ "let var x := 1",
                  "var x := 2",
                  "in x end"
                ]
            )
            $ Just Types.Int,
          typeCheckTest'
            ( unwords
                [ "let var x := 1",
                  "var x := \"hello\"",
                  "in x end"
                ]
            )
            $ Just Types.String,
          typeCheckTest' "let type t = int in 0 end" $ Just Types.Int,
          typeCheckTest' "let type list = {hd: int, tail: list} in 0 end" $ Just Types.Int
        ],
      testCaseSteps "tiger valid testcases" $ \step -> do
        tests <- TigerTests.validTestCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right e ->
              case checkProg e of
                Left err' -> assertFailure $ show err'
                Right {} -> pure (),
      testCaseSteps "tiger error testcases" $ \step -> do
        tests <- TigerTests.errorTestCases
        forM_ tests $ \(f, s) -> do
          step f
          case parse f s of
            Left err -> assertFailure err
            Right e ->
              case checkProg e of
                Left err' -> pure ()
                Right {} -> assertFailure "expected failure"
    ]
