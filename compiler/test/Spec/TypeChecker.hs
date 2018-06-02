module Spec.TypeChecker where

import Test.Hspec
import Test.QuickCheck
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, execStateT)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import TypeChecker
import AST
import Types

transExpr' :: Expr -> Either TError Ty
transExpr' e = fst <$> runStateT (snd <$> transExpr e) initEnv

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
      insertVar "x" (Array Int)
      transExpr $ VExpr $ ArrayVar (SimpleVar "x") (IExpr 1)
   `shouldBe` Right Int

  it "typechecks record vars 1" $ do
    getTy $ do
      insertVar "x" $ Record [("field1", Int), ("field2", String)]
      transExpr $ VExpr $ FieldVar (SimpleVar "x") "field1"
   `shouldBe` Right Int

  it "typechecks record vars 2" $ do
    getTy $ do
      insertVar "x" $ Record [("field1", Int), ("field2", String)]
      transExpr $ VExpr $ FieldVar (SimpleVar "x") "field2"
    `shouldBe` Right String

  it "adds var typeless decs to the env" $ do
     let vDec       = VarDec $ VarDef "x" Nothing (IExpr 5)
     let Right vEnv = fst <$> execStateT (transDec vDec) initEnv
     M.lookup "x" vEnv `shouldBe` Just (VarEntry Int)

  it "adds var typed decs to the env" $ do
     let vDec       = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     let Right vEnv = fst <$> execStateT (transDec vDec) initEnv
     M.lookup "x" vEnv `shouldBe` Just (VarEntry Int)

  it "rejects incorrectly typed vars" $ do
     let vDec = VarDec $ VarDef "x" (Just "string") (IExpr 5)
     let s    = fst <$> execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "rejects expressions of type nil without a type" $ do
     let vDec = VarDec $ VarDef "x" Nothing NilExpr
     let s    = fst <$> execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "rejects vars of type nil that aren't records" $ do
     let vDec = VarDec $ VarDef "x" (Just "int") NilExpr
     let s    = fst <$> execStateT (transDec vDec) initEnv
     isLeft s `shouldBe` True

  it "adds DataConst types to the env" $ do
     let tDec       = TypeDec [Type "fooT" (DataConst "int")]
     let Right tEnv = snd <$> execStateT (transDec tDec) initEnv
     M.lookup "fooT" tEnv `shouldBe` Just Int

  it "adds DataConst types to the env 2" $ do
     let tDec       = TypeDec [Type "fooT" (DataConst "int")]
     let tDec2      = TypeDec [Type "derpT" (DataConst "fooT")]
     let Right tEnv = snd <$> execStateT (transDec tDec >> transDec tDec2) initEnv
     M.lookup "derpT" tEnv `shouldBe` Just Int

  it "adds RecordType types to the env" $ do
     let tDec = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "string"])]
     let Right tEnv = snd <$> execStateT (transDec tDec) initEnv
     M.lookup "recT" tEnv `shouldBe` Just (Record [("field1", Int), ("field2", String)])

  it "adds ArrayType types to the env" $ do
     let tDec = TypeDec [Type "arrayT" (ArrayType "int")]
     let Right tEnv = snd <$> execStateT (transDec tDec) initEnv
     M.lookup "arrayT" tEnv `shouldBe` Just (Array Int)

  it "accepts record vars initialized with expressions of type nil" $ do
     let tDec = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "string"])]
     let vDec = VarDec $ VarDef "x" (Just  "recT") NilExpr
     let Right vEnv = fst <$> execStateT (transDec tDec >> transDec vDec) initEnv
     M.lookup "x" vEnv `shouldBe` Just (VarEntry (Record [("field1", Int), ("field2", String)]))

  it "adds function declaration types to the env" $ do
    let fDec = FunDec [FunDef "f" ["x" |: "int", "y" |: "string"] (Just "string") (SExpr "foo")]
    let Right vEnv = fst <$> execStateT (transDec fDec) initEnv
    M.lookup "f" vEnv `shouldBe` Just (FunEntry [Int,String] String)

  it "rejects procedures whose bodies don't type match" $ do
    let fDec = FunDec [FunDef "f" ["x" |: "int", "y" |: "string"] Nothing (SExpr "foo")]
    let s    = execStateT (transDec fDec) initEnv
    isLeft s `shouldBe` True

  it "adds procedure declaration types to the env" $ do
    let fDec = FunDec [FunDef "f" ["x" |: "int", "y" |: "string"] Nothing
                       (If (BExpr Equal (IExpr 1) (IExpr 1)) Break)]
    let Right vEnv = fst <$> execStateT (transDec fDec) initEnv
    M.lookup "f" vEnv `shouldBe` Just (FunEntry [Int,String] Unit)
