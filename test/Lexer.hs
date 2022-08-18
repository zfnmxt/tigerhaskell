module Lexer where

import Test.Tasty
import Test.Tasty.HUnit

test_unitTests :: TestTree
test_unitTests =
  testGroup
    "Unit Tests"
    [ testCase "String comparison 1" $
        assertEqual "description" "OK" "OK",
      testCase "String comparison 2" $ -- should fail
        assertEqual "description" "fail" "fail!"
    ]
