module Parser where

import qualified Data.Map as M
import qualified Data.Set as S
import Parser.Grammar
import Parser.Types
import Test.Tasty
import Test.Tasty.HUnit
import Util

test_nullable_first_follow :: TestTree
test_nullable_first_follow =
  testGroup
    "grammar3.12"
    [ testCase "nullable[X]" $ nullable g (N 'X') @?= True,
      testCase "nullable[Y]" $ nullable g (N 'Y') @?= True,
      testCase "nullable[Z]" $ nullable g (N 'Z') @?= False,
      testCase "FIRST[X]" $ first_ g (N 'X') @?= S.fromList [T 'a', T 'c'],
      testCase "FIRST[Y]" $ first_ g (N 'Y') @?= S.fromList [T 'c'],
      testCase "FIRST[Z]" $ first_ g (N 'Z') @?= S.fromList [T 'a', T 'c', T 'd'],
      testCase "FOLLOW[X]" $ follow g (N 'X') @?= S.fromList [T 'a', T 'c', T 'd'],
      testCase "FOLLOW[Y]" $ follow g (N 'Y') @?= S.fromList [T 'a', T 'c', T 'd'],
      testCase "FOLLOW[Z]" $ follow g (N 'Z') @?= S.fromList []
    ]
  where
    g =
      mkGrammar
        [ ('Z', "d"),
          ('Z', "XYZ"),
          ('Y', ""),
          ('Y', "c"),
          ('X', "Y"),
          ('X', "a")
        ]
