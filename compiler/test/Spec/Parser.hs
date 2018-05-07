module Spec.Parser where

import Test.Hspec
import Test.QuickCheck

import Parser
import DumbParser
import AST

testEnv :: Env 
testEnv = Env 0 0

testParse :: TigerP a -> String -> a
testParse p s = case runParser p testEnv s of
  Left  _ -> error "oops"
  Right x -> fst x

testParseE :: String -> Either TigerError Expr
testParseE s = case runParser exprP testEnv s of
  Left  x -> (Left "")
  Right x -> case (snd (snd x)) of
               "" -> (Right (fst x))
               _  -> Left ""

parserTests :: SpecWith ()
parserTests = do
  baseTests

baseTests :: SpecWith ()
baseTests = describe "Basic parser tests" $ do
  it "parses string literals" $ do
    testParseE "  \"foo\"" `shouldBe` Right (SExpr "foo")
    testParseE "\"\\\"foo\\\"\"" `shouldBe` Right (SExpr "\"foo\"")
    testParseE "\"\\234\"" `shouldBe` Right (SExpr "\234")
    testParseE "\"\\234\\124\"" `shouldBe` Right (SExpr "\234\124")

  it "parses integer literals" $ do
    testParseE "55" `shouldBe` Right (IExpr 55)

  it "parses no value" $ do
    testParseE "()" `shouldBe` Right NoValue

  it "parses negation" $ do
    testParseE "-55" `shouldBe` Right (NExpr (IExpr 55))

  it "parses multiplication and division" $ do
    testParseE "1 * 2" `shouldBe` Right (BExpr Mult (IExpr 1) (IExpr 2))
    testParseE "1 / 2" `shouldBe` Right (BExpr Div (IExpr 1) (IExpr 2))
    testParseE "1 * 2 * 3" `shouldBe`
      Right (BExpr Mult (BExpr Mult (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 / 2 / 3" `shouldBe`
      Right (BExpr Div (BExpr Div (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 / 2 * 3" `shouldBe`
      Right (BExpr Mult (BExpr Div (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 * 2 / 3" `shouldBe`
      Right (BExpr Div (BExpr Mult (IExpr 1) (IExpr 2)) (IExpr 3))

  it "parses addition and subtraction" $ do
    testParseE "1 + 2" `shouldBe` Right (BExpr Add (IExpr 1) (IExpr 2))
    testParseE "1 - 2" `shouldBe` Right (BExpr Sub (IExpr 1) (IExpr 2))
    testParseE "1 + 2 + 3" `shouldBe`
      Right (BExpr Add (BExpr Add (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 - 2 - 3" `shouldBe`
      Right (BExpr Sub (BExpr Sub (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 - 2 + 3" `shouldBe`
      Right (BExpr Add (BExpr Sub (IExpr 1) (IExpr 2)) (IExpr 3))
    testParseE "1 + 2 - 3" `shouldBe`
      Right (BExpr Sub (BExpr Add (IExpr 1) (IExpr 2)) (IExpr 3))

  it "parses comparison operators" $ do
    testParseE "1 = 2"  `shouldBe` Right (BExpr Equal (IExpr 1) (IExpr 2))
    testParseE "1 <> 2" `shouldBe` Right (BExpr NEqual (IExpr 1) (IExpr 2))
    testParseE "1 > 2"  `shouldBe` Right (BExpr Gt (IExpr 1) (IExpr 2))
    testParseE "1 < 2"  `shouldBe` Right (BExpr Lt (IExpr 1) (IExpr 2))
    testParseE "1 >= 2" `shouldBe` Right (BExpr GTE (IExpr 1) (IExpr 2))
    testParseE "1 <= 2" `shouldBe` Right (BExpr LTE (IExpr 1) (IExpr 2))
    testParseE "1 = 2 = 3" `shouldBe` Left ""
    testParseE "1 = (2 = 3)" `shouldBe`
      Right (BExpr Equal (IExpr 1) (BExpr Equal (IExpr 2) (IExpr 3)))
    testParseE "(1 = 2) = 3" `shouldBe`
      Right (BExpr Equal (BExpr Equal (IExpr 1) (IExpr 2)) (IExpr 3))






