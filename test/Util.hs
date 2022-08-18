module Util where

import Control.Monad
import Data.List (sort)
import System.Directory

data TestFile = TestFile
  { name :: String,
    contents :: String
  }
  deriving (Show, Eq, Ord)

testFiles :: IO [TestFile]
testFiles = do
  fs <- listDirectory "test/testcases"
  tfs <- forM fs $ \n -> do
    c <- readFile $ "test/testcases/" ++ n
    pure $ TestFile n c
  pure $ sort tfs
