module Spec.Parser where

import Test.Hspec
import Test.QuickCheck
import Spec.AppelTigFiles

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

testParseD :: String -> Either TigerError Dec
testParseD s = case runParser decP testEnv s of
  Left  x -> (Left "")
  Right x -> case (snd (snd x)) of
               "" -> (Right (fst x))
               _  -> Left ""

parserTests :: SpecWith ()
parserTests = do
  baseTests
  appelTests

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

  it "parses function calls" $ do
    testParseE "func()" `shouldBe` Right (FCall "func" [])
    testParseE "func(1, \"foo\")" `shouldBe` Right (FCall "func" [IExpr 1, SExpr "foo"])
    testParseE "func(1,)" `shouldBe` Left ""

  it "parses break statements" $ do
    testParseE "break" `shouldBe` Right (Break)

  it "parses if-then statements" $ do
    testParseE "if 1 > 2 then break" `shouldBe`
      Right (If (BExpr Gt (IExpr 1) (IExpr 2)) (Break))
    testParseE "if 1 > 2 then if 1 > 2 then break" `shouldBe`
      Right (If (BExpr Gt (IExpr 1) (IExpr 2)) (If (BExpr Gt (IExpr 1) (IExpr 2)) (Break)))

  it "parses if-then-else statements" $ do
    testParseE "if 1 > 2 then break else 1 < 2" `shouldBe`
      Right (IfE (BExpr Gt (IExpr 1) (IExpr 2)) (Break) (BExpr Lt (IExpr 1) (IExpr 2)))

  it "parses while statements" $ do
    testParseE "while 1 > 2 do break" `shouldBe`
      Right (While (BExpr Gt (IExpr 1) (IExpr 2)) Break)

  it "parses l-values" $ do
    testParseE "foo123" `shouldBe` Right (LExpr (LId "foo123"))
    testParseE "123foo" `shouldBe` Left ""
    testParseE "myRecord.foo" `shouldBe` Right (LExpr (LField (LId "myRecord") "foo"))
    testParseE "myArray[5]" `shouldBe` Right (LExpr (LArray (LId "myArray") (IExpr 5)))
    testParseE "myArray[]" `shouldBe` Left ""
    testParseE "myRecord.myArray[5]" `shouldBe`
      Right (LExpr (LArray (LField (LId "myRecord") "myArray") (IExpr 5)))

  it "parses for statements" $ do
    testParseE "for i := 0 to 5 do break" `shouldBe`
      Right (For (Assign (LId "i") (IExpr 0)) (IExpr 5) Break)

  it "parses type declarations" $ do
    testParseD "type singletonType = singletonType" `shouldBe`
      Right (TypeDec(Type "singletonType" (DataConst "singletonType") ))
    testParseD "type intlist = array of int" `shouldBe`
      Right (TypeDec(Type "intlist" (ArrayType "int") ))
    testParseD "type tree = {key: int, children: treelist}" `shouldBe`
      Right (TypeDec(Type "tree" (RecordType [TypeField "key" "int", TypeField "children" "treelist"])))
    testParseD "type emptyRecord = {}" `shouldBe`
      Right (TypeDec(Type "emptyRecord" (RecordType [])))

  it "parses variable declarations" $ do
    testParseD "var x := 5" `shouldBe` Right (VarDec (Var "x" Nothing (IExpr 5)))
    testParseD "var x : int := 5" `shouldBe` Right (VarDec (Var "x" (Just "int") (IExpr 5)))
    testParseD "var x : := 5" `shouldBe` Left ""

  it "parses function declarations" $ do
    testParseD "function myFunc() = 5" `shouldBe`
      Right (FunDec (Fun "myFunc" [] Nothing (IExpr 5)))
    testParseD "function myFunc () : int = 5" `shouldBe`
      Right (FunDec (Fun "myFunc" [] (Just "int") (IExpr 5)))
    testParseD "function myFunc(arg1: int, arg2: string, arg3:foo) : int = 5" `shouldBe`
      Right (FunDec (Fun "myFunc" [TypeField "arg1" "int", TypeField "arg2" "string", TypeField "arg3" "foo"]
                     (Just "int") (IExpr 5)))

  it "parses let statements" $ do
    testParseE "let in 5 end" `shouldBe`
      Right (Let [] [IExpr 5])
    testParseE "let in end" `shouldBe`
      Right (Let [] [])
    testParseE "let var x := 5 \n var y := 6 in x + y end" `shouldBe`
      Right (Let [(VarDec (Var "x" Nothing (IExpr 5))), (VarDec (Var "y" Nothing (IExpr 6)))]
                 [BExpr Add (LExpr (LId "x")) (LExpr (LId "y"))])

appelTests :: SpecWith ()
appelTests = describe "tests using appel's .tig files" $ do
  it "parses test1 correctly" $ do
    let letDecs = [ TypeDec (Type "arrtype" (ArrayType "int"))
              , VarDec ( Var "arr1" (Just "arrtype")
                         (Array "arrtype" (IExpr 10) (IExpr 0))
                       )
              ]
    testParseE test1 `shouldBe` Right (Let letDecs [(LExpr (LId "arr1"))])

  it "parses test2 correctly" $ do
    let letDecs = [ TypeDec (Type "myint" (DataConst "int"))
                  , TypeDec (Type "arrtype" (ArrayType "myint"))
                  , VarDec ( Var "arr1" (Just "arrtype")
                             (Array "arrtype" (IExpr 10) (IExpr 0))
                           )
                 ]
    testParseE test2 `shouldBe` Right (Let letDecs [(LExpr (LId "arr1"))])

  it "parses test3 correctly" $ do
    let letDecs = [ TypeDec (Type "rectype" (RecordType [TypeField "name" "string", TypeField "age" "int"]))
                  , VarDec ( Var "rec1" (Just "rectype")
                             (Record "rectype" [RecordField "name" (SExpr "Nobody"), RecordField "age" (IExpr 1000)])
                           )
                 ]
    testParseE test3 `shouldBe` Right (Let letDecs
                                          [ Assign (LField (LId "rec1") "name") (SExpr "Somebody"),
                                            LExpr (LId "rec1")
                                          ]
                                      )


