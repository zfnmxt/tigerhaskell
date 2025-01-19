module Lexer where

import Control.Monad
import Data.Either
import Lexer.Lexer
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Prelude hiding (lex)

test_tigerTestCases :: IO (TestTree)
test_tigerTestCases = do
  files <- testFiles
  pure $ testGroup "Included testcases" $ map func files
  where
    func :: TestFile -> TestTree
    func f = testCase (name f) $ assertBool "" $ isRight $ lex (contents f)
