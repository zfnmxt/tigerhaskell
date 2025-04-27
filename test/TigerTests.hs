module TigerTests (testCases) where

import System.Directory
import System.FilePath

testCases :: IO [(FilePath, String)]
testCases =
  mapM (\f -> (f,) <$> readFile f) =<< files
  where
    dir = "tiger/testcases/"
    files = map (dir <>) . filter ((== ".tig") . takeExtension) <$> listDirectory dir
