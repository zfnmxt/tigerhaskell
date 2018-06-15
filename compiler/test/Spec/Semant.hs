{-# LANGUAGE RecordWildCards #-}

module Spec.Semant where

import Test.Hspec
import Test.QuickCheck
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, execStateT)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Spec.AppelTigFiles
import Spec.Parser (testParseE)

import Parser
import Semant
import AST
import Types

parseAndTy :: String -> Either TError Ty
parseAndTy s = case testParseE s of
  Left _ -> Left "oops"
  Right x -> getTy (transExpr x)

transExprEnv :: Expr -> Env -> Either TError Ty
transExprEnv e env = fst <$> runStateT (snd <$> transExpr e) env

getTy :: CheckerState TExprTy -> Either TError Ty
getTy cs = fst <$> runStateT (snd <$> cs) initEnv

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

typeCheckerTests :: SpecWith ()
typeCheckerTests = do
  baseTests
  appelTests

baseTests :: SpecWith ()
baseTests = describe "Basic type checker tests" $ do
  it "typechecks strings" $ do
    getTy (transExpr (SExpr "foo")) `shouldBe` Right String

  it "typechecks ints" $ do
    getTy (transExpr (IExpr 5)) `shouldBe` Right Int

  it "typechecks simple vars" $ do
    getTy (insertVar "x" Int >> transExpr (VExpr (SimpleVar "x"))) `shouldBe` Right Int

  it "typechecks array vars" $ do
    getTy $ do
      insertVar "x" (Array "foo" Int)
      transExpr $ VExpr $ ArrayVar (SimpleVar "x") (IExpr 1)
   `shouldBe` Right Int

  it "typechecks record vars 1" $ do
    getTy $ do
      insertVar "x" $ Record "foo" (Just [("field1", Int), ("field2", String)])
      transExpr $ VExpr $ FieldVar (SimpleVar "x") "field1"
   `shouldBe` Right Int

  it "typechecks record vars 2" $ do
    getTy $ do
      insertVar "x" $ Record "foo" (Just [("field1", Int), ("field2", String)])
      transExpr $ VExpr $ FieldVar (SimpleVar "x") "field2"
    `shouldBe` Right String

  it "adds var typeless decs to the env" $ do
     let vDec          = VarDec $ VarDef "x" Nothing (IExpr 5)
     let Right Env{..} = execStateT (transDec vDec) initEnv
     M.lookup "x" _envV `shouldBe` Just (VarEntry Int)

  it "adds var typed decs to the env" $ do
     let vDec          = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     let Right Env{..} = execStateT (transDec vDec) initEnv
     M.lookup "x" _envV `shouldBe` Just (VarEntry Int)

  it "rejects incorrectly typed vars" $ do
     let vDec = VarDec $ VarDef "x" (Just "string") (IExpr 5)
     let s    = execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "rejects expressions of type nil without a type" $ do
     let vDec = VarDec $ VarDef "x" Nothing NilExpr
     let s    = execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "rejects vars of type nil that aren't records" $ do
     let vDec = VarDec $ VarDef "x" (Just "int") NilExpr
     let s    = execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "adds DataConst types to the env" $ do
     let tDec       = TypeDec [Type "fooT" (DataConst "int")]
     let Right Env{..} = execStateT (transDec tDec) initEnv
     M.lookup "fooT" _envT `shouldBe` Just Int

  it "adds DataConst types to the env 2" $ do
     let tDec          = TypeDec [Type "fooT" (DataConst "int")]
     let tDec2         = TypeDec [Type "derpT" (DataConst "fooT")]
     let Right Env{..} = execStateT (transDec tDec >> transDec tDec2) initEnv
     M.lookup "derpT" _envT `shouldBe` Just Int

  it "adds RecordType types to the env" $ do
     let tDec          = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "string"])]
     let Right Env{..} = execStateT (transDec tDec) initEnv
     M.lookup "recT" _envT `shouldBe` Just (Record "recT" (Just [("field1", Int), ("field2", String)]))

  it "adds ArrayType types to the env" $ do
     let tDec            = TypeDec [Type "arrayT" (ArrayType "int")]
     let Right Env{..} = execStateT (transDec tDec) initEnv
     M.lookup "arrayT" _envT `shouldBe` Just (Array "arrayT" Int)

  it "accepts record vars initialized with expressions of type nil" $ do
     let tDec = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "string"])]
     let vDec = VarDec $ VarDef "x" (Just  "recT") NilExpr
     let Right Env{..} = execStateT (transDec tDec >> transDec vDec) initEnv
     M.lookup "x" _envV `shouldBe` Just (VarEntry (Record "recT" (Just [("field1", Int), ("field2", String)])))

  it "adds function declaration types to the env" $ do
    let fDec = FunDec [FunDef "f" [Field "x" "int" True, Field "y" "string" True] (Just "string") (SExpr "foo")]
    let Right Env{..} = execStateT (transDec fDec) initEnv
    M.lookup "f" _envV `shouldBe` Just (FunEntry [Int,String] String)

  it "rejects procedures whose bodies don't type match" $ do
    let fDec = FunDec [FunDef "f" [Field "x" "int" True, Field "y" "string" True] Nothing (SExpr "foo")]
    let s    = execStateT (transDec fDec) initEnv
    isLeft s `shouldBe` True

  it "adds procedure declaration types to the env" $ do
    let fDec = FunDec [FunDef "f" [Field "x" "int" True, Field "y" "string" True] Nothing
                       (If (BExpr Equal (IExpr 1) (IExpr 1)) UnitExpr)]
    let Right Env{..} = execStateT (transDec fDec) initEnv
    M.lookup "f" _envV `shouldBe` Just (FunEntry [Int,String] Unit)

  it "accepts well-typed if statements" $ do
    getTy (transExpr  (If (IExpr 0) UnitExpr)) `shouldBe` Right Unit

  it "accepts well-typed ifE statements" $ do
    getTy (transExpr  (IfE (IExpr 0) (IExpr 1) (IExpr 2))) `shouldBe` Right Int

  it "rejects bad if statements" $ do
    let s = execStateT (transExpr (If (IExpr 0) (IExpr 1))) initEnv
    isLeft s `shouldBe` True

  it "accepts well-typed While statements" $ do
    getTy (transExpr  (While (IExpr 0) Break )) `shouldBe` Right Unit

  it "accepts well-typed For statements" $ do
    getTy (transExpr  (For (Assign (SimpleVar "x") (IExpr 0)) (IExpr 5) Break)) `shouldBe` Right Unit

  it "correctly binds type decs in let expressions" $ do
     let tDec       = TypeDec [Type "fooT" (DataConst "int")]
     let tDec2      = TypeDec [Type "derpT" (DataConst "fooT")]
     let vDec       = VarDec $ VarDef "x" (Just "derpT") (IExpr 5)
     let letExpr    = Let [tDec, tDec2, vDec] [VExpr (SimpleVar "x")]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly binds function decs in let expressions" $ do
     let tDec       = TypeDec [Type "fooT" (DataConst "int")]
     let vDec       = VarDec $ VarDef "x" (Just "fooT") (IExpr 5)
     let fDec       = FunDec [FunDef "f" [Field "x" "fooT" True, Field "y" "string" True]
                              (Just "fooT") (VExpr (SimpleVar "x"))]
     let letExpr    = Let [tDec, vDec, fDec] [FCall "f" [IExpr 1, SExpr "blah"]]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly checks mutually-recursive function declarations" $ do
     let fDec    = FunDec [ FunDef "f" [Field "x" "int" True] (Just "int") (FCall "g" [IExpr 1])
                          , FunDef "g" [Field "x" "int" True] (Just "int") (FCall "f" [IExpr 1])
                          ]
     let letExpr = Let [fDec] [FCall "g" [IExpr 1]]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly checks mutually-recursive record type declarations" $ do
     let tDec = TypeDec [Type "rec1" (RecordType ["field1" |: "int", "field2" |: "rec2"])
                        ,Type "rec2" (RecordType ["field1" |: "int", "field2" |: "rec1"])
                        ]
     let letExpr = Let [tDec] [IExpr 1]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly checks recursive function declarations" $ do
     let fDec    = FunDec [ FunDef "f" [Field "x" "int" True] (Just "int") (FCall "f" [IExpr 1])
                          ]
     let letExpr = Let [fDec] [FCall "f" [IExpr 1]]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly checks recursive record type declarations" $ do
     let tDec = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "recT"])
                        ]
     let letExpr = Let [tDec] [IExpr 1]
     getTy (transExpr letExpr) `shouldBe` Right Int

  it "correctly checks record type declarations 2" $ do
     let tDecs = TypeDec [ Type "recT" (RecordType ["field1" |: "int", "field2" |: "recT"])
                         , Type "arrayT" (ArrayType "recT")
                         ]
     let Right Env{..} = execStateT (transDec tDecs) initEnv
     M.lookup "arrayT" _envT `shouldBe` Just (Array "arrayT" (Record "recT" Nothing ))

  it "rejects break statements outside of for/while loops" $ do
    let s = evalStateT (transExpr (If (IExpr 0) Break)) initEnv
    isLeft s `shouldBe` True

  it "rejects break statements outside of for/while loops 2" $ do
    let s = evalStateT (transExpr Break) initEnv
    isLeft s `shouldBe` True

appelTests :: SpecWith ()
appelTests = describe "Type checker tests using appel's .tig files" $ do
  it "checks test1" $ do
    parseAndTy test1 `shouldBe` Right (Array "arrtype" Int)
  it "checks test2" $ do
    parseAndTy test2 `shouldBe` Right (Array "arrtype" Int)
  it "checks test3" $ do
    parseAndTy test3 `shouldBe` Right (Record "rectype" (Just [("name", String), ("age", Int)]))
  it "checks test4" $ do
    parseAndTy test4 `shouldBe` Right Int
  it "checks test5" $ do
    parseAndTy test5 `shouldBe` Right (Record "intlist" (Just [("hd", Int), ("tl", Record "intlist" Nothing)]))
  it "checks test6" $ do
    parseAndTy test6 `shouldBe` Right Unit
  it "checks test7" $ do
    parseAndTy test7 `shouldBe` Right Int
  it "checks test8" $ do
    parseAndTy test8 `shouldBe` Right Int
  it "checks test9" $ do
    isLeft (parseAndTy test9) `shouldBe` True
  it "checks test10" $ do
    isLeft (parseAndTy test10) `shouldBe` True
  it "checks test11" $ do
    isLeft (parseAndTy test11) `shouldBe` True
  it "checks test12" $ do
    parseAndTy test12 `shouldBe` Right Unit
  it "checks test13" $ do
    isLeft (parseAndTy test13) `shouldBe` True
  it "checks test14" $ do
    isLeft (parseAndTy test14) `shouldBe` True
  it "checks test15" $ do
    isLeft (parseAndTy test15) `shouldBe` True
  it "checks test16" $ do
    isLeft (parseAndTy test16) `shouldBe` True
  it "checks test17" $ do
    isLeft (parseAndTy test17) `shouldBe` True
  it "checks test18" $ do
    isLeft (parseAndTy test18) `shouldBe` True
  it "checks test19" $ do
    isLeft (parseAndTy test19) `shouldBe` True
  it "checks test20" $ do
    isLeft (parseAndTy test20) `shouldBe` True
  it "checks test21" $ do
    isLeft (parseAndTy test21) `shouldBe` True
  it "checks test22" $ do
    isLeft (parseAndTy test22) `shouldBe` True
  it "checks test23" $ do
    isLeft (parseAndTy test23) `shouldBe` True
  it "checks test24" $ do
    isLeft (parseAndTy test24) `shouldBe` True
  it "checks test25" $ do
    isLeft (parseAndTy test25) `shouldBe` True
  it "checks test26" $ do
    isLeft (parseAndTy test26) `shouldBe` True
  it "checks test27" $ do
    parseAndTy test27 `shouldBe` Right Int
  it "checks test28" $ do
    isLeft (parseAndTy test28) `shouldBe` True
  it "checks test29" $ do
    isLeft (parseAndTy test29) `shouldBe` True
  it "checks test30" $ do
    parseAndTy test30 `shouldBe` Right Int
  it "checks test31" $ do
    isLeft (parseAndTy test31) `shouldBe` True
  it "checks test32" $ do
    isLeft (parseAndTy test32) `shouldBe` True
  it "checks test33" $ do
    isLeft (parseAndTy test33) `shouldBe` True
  it "checks test34" $ do
    isLeft (parseAndTy test34) `shouldBe` True
  it "checks test35" $ do
    isLeft (parseAndTy test35) `shouldBe` True
  it "checks test36" $ do
    isLeft (parseAndTy test36) `shouldBe` True
  it "checks test37" $ do
    parseAndTy test37 `shouldBe` Right Int
  it "checks test38" $ do
    isLeft (parseAndTy test38) `shouldBe` True
  it "checks test39" $ do
    isLeft (parseAndTy test39) `shouldBe` True
  it "checks test40" $ do
    isLeft (parseAndTy test40) `shouldBe` True
  it "checks test41" $ do
    parseAndTy test41 `shouldBe` Right Int
  it "checks test42" $ do
    parseAndTy test42 `shouldBe` Right Unit
  it "checks test43" $ do
    isLeft (parseAndTy test43) `shouldBe` True
  it "checks test44" $ do
    parseAndTy test44 `shouldBe` Right Unit
  it "checks test45" $ do
    isLeft (parseAndTy test45) `shouldBe` True
  it "checks test46" $ do
    parseAndTy test46 `shouldBe` Right Int
  it "checks test47" $ do
    parseAndTy test47 `shouldBe` Right Int
  it "checks test48" $ do
    parseAndTy test48 `shouldBe` Right Int
  it "checks test49" $ do
    isLeft (parseAndTy test49) `shouldBe` True
  it "checks queens" $ do
    parseAndTy queens `shouldBe` Right Unit
  it "checks merge" $ do
    parseAndTy queens `shouldBe` Right Unit
