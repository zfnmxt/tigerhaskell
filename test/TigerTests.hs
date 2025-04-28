module TigerTests (validTestCases, errorTestCases, testCases) where

import System.Directory
import System.FilePath

testCases :: IO [(FilePath, String)]
testCases = (++) <$> validTestCases <*> errorTestCases

errorTestCases :: IO [(FilePath, String)]
errorTestCases = getTestCases "tiger/testcases/error/"

validTestCases :: IO [(FilePath, String)]
validTestCases = getTestCases "tiger/testcases/valid/"

getTestCases :: FilePath -> IO [(FilePath, String)]
getTestCases dir =
  mapM (\f -> (f,) <$> readFile f) =<< files
  where
    files = map (dir <>) . filter ((== ".tig") . takeExtension) <$> listDirectory dir
