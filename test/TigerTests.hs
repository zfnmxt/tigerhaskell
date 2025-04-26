module TigerTests (testCases) where

import System.Directory

testCases :: IO [FilePath]
testCases = listDirectory "tiger/testcases"
