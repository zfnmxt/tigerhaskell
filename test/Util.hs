module Util where

import Control.Monad
import Data.List (sort)
import System.Directory
import System.IO

data TestFile = TestFile
  { name :: String,
    contents :: String
  }
  deriving (Show, Eq, Ord)

testFiles :: IO [TestFile]
testFiles = do
  files <- listDirectory "test/testcases"
  testFiles <- forM files $ \n -> do
    c <- readFile $ "test/testcases/" ++ n
    pure $ TestFile n c
  pure $ sort testFiles
