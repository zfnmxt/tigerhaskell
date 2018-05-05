module Spec.Parser where

import Test.Hspec
import Test.QuickCheck

import Parser
import DumbParser

parserTests :: SpecWith ()
parserTests = do
  baseTests

baseTests :: SpecWith ()
baseTests = describe "Basic parser tests" $ do
  it "parses string literals correctly" $ do
   
