module Spec.GenAppelTigFiles where

import Data.List (intercalate)

appelDir = "../../../appel/tests/"
firstTest = 1
lastTest  = 49

genTig :: Int -> IO String
genTig num = readFile $ appelDir ++ "test" ++ show num ++ ".tig"

genEntry :: Int -> IO String
genEntry num = do
  text <- genTig num
  return $ "test" ++ show num ++ "=" ++ show text

genEntries :: Int -> Int -> IO String
genEntries min max = intercalate "\n\n" <$> mapM genEntry [min..max]

main :: IO ()
main = do
  text <- genEntries firstTest lastTest
  writeFile "AppelTigFiles.hs" ("module Spec.AppelTigFiles where \n" ++ text)
