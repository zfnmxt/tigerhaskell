{-# LANGUAGE RecordWildCards #-}

module Spec.Translate where

import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, execStateT,
                                                 runStateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.Map.Lazy                  (Map)
import qualified Data.Map.Lazy                  as M
import           Test.Hspec
import           Test.QuickCheck

import           Spec.AppelTigFiles
import           Spec.Parser                    (testParseE)
import           Spec.Semant

import           AST
import           Frame
import           Parser
import           Registers
import           Semant
import           STEnv
import           Temp
import           Translate
import           Tree
import qualified Tree                           as T
import           Types
import           RunHavm


havmExpr :: Expr -> IO String
havmExpr e = do
  let Right (TExpr (Ex tExp) _) = evalStateT' (transExpr e)
  runHavm $ "label main" ++ genHavmExp 0 tExp


evalStateT' m = evalStateT m initEnv
execStateT' m = execStateT m initEnv

parseAndTran :: String -> Either TError TExpr
parseAndTran s = case testParseE s of
  Left _  -> Left "oops"
  Right x -> evalStateT'(transExpr x)

translateTests :: SpecWith ()
translateTests = do
  stackFrameTests
  treeTests

stackFrameTests :: SpecWith ()
stackFrameTests =
  describe "Basic stack frame tests" $ do
  it "basic nesting level" $ do
    let fDec = FunDec [FunDef "f" [Field "x" "int" True, Field "y" "string" True] (Just "string") (SExpr "foo")]
    let Right Env{..} = execStateT (transDec fDec) initEnv
    getVLevel <$> (M.lookup "f"_envV) `shouldBe` Just 2

  it "deeper nesting level" $ do
    let fBase = FunDec [FunDef "f" [Field "x" "int" True] (Just "string") (SExpr "foo")]
    let gDec  = FunDec [FunDef "g" [Field "x" "int" True] (Just "string") (Let [fBase] [SExpr "foo"])]
    let hDec  = FunDec [FunDef "h" [Field "x" "int" True] (Just "string") (Let [gDec] [SExpr "foo"])]
    let Right Env{..} = execStateT (transDec hDec) initEnv
    getVLevel <$> (M.lookup "h"_envV) `shouldBe` Just 2

treeTests :: SpecWith ()
treeTests =
  describe "Basic tree tests" $ do
  return ()
  --it "simple var" $ do
     --let vDec    = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     --let letExpr = Let [vDec] [VExpr (SimpleVar "x")]

--     transExpr letExpr `shouldBe` Right (TExpr (Ex $ Const 0) Int)

--  it "two simple vars" $ do
--     let vDec1   = VarDec $ VarDef "x" (Just "int") (IExpr 5)
--     let vDec2   = VarDec $ VarDef "y" (Just "int") (IExpr 1)
--     let letExpr = Let [vDec1, vDec2] [ExprSeq [VExpr (SimpleVar "x"), VExpr (SimpleVar "y")]]
--     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr (Ex $ Const 0) Int)
--  it "function call" $ do
--     let fDec    = FunDec [FunDef "f" [Field "y" "int" True] (Just "int") (IExpr 1)]
--     let letExpr = Let [fDec] [FCall "f" [IExpr 1]]
--     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr (Ex $ Const 0) Int)
--  it "nested simple var" $ do
--     let vDec    = VarDec $ VarDef "x" (Just "int") (IExpr 5)
--     let fDec    = FunDec [FunDef "f" [Field "y" "int" True] (Just "int") (VExpr (SimpleVar "y"))]
--     let letExpr = Let [vDec, fDec] [FCall "f" [VExpr (SimpleVar "x")]]
--     let vLoc    = Mem (BinOp T.Plus (Const (-4)) (Mem (BinOp T.Plus (Const 0) (IReg (RTemp RBX)))))
--     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr (Ex $ Const 0) Int)
--  it "simple array" $ do
--     let tDec    = TypeDec [Type "arrayT" (ArrayType "int")]
--     let vDec    = VarDec $ VarDef "x" (Just "arrayT") (ArrayExpr "arrayT" (IExpr 5) (IExpr 2))
--     let aExp    = VExpr $ ArrayVar (SimpleVar "x") (IExpr 1)
--     let letExpr = Let [tDec, vDec] [aExp]
--     let tExpr   = Ex (Mem (BinOp Plus (Mem (BinOp Plus (Const (-4)) (IReg (RTemp RBX)))) (BinOp 
--                    Mul (Const 1) (Const 4))))                                                                     
--     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr (Ex $ Const 0) Int)
--  it "simple record" $ do
--     let tDec    = TypeDec [Type "recT" (RecordType ["field1" |: "int", "field2" |: "string"])]
--     let vDec    = VarDec $ VarDef "x" (Just "recT") (RecordExpr "recT"
--                                                        ["field1" |. IExpr 1, "field2" |. SExpr "foo"])
--     let rExp    = VExpr $ FieldVar (SimpleVar "x") "field2"
--     let letExpr = Let [tDec, vDec] [rExp]
--     let tExpr   = Ex (Mem (BinOp Plus (Mem (BinOp Plus (Const (-4)) (IReg (RTemp RBX)))) (BinOp 
--                    Mul (Const 1) (Const 4))))                                                                     
--     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr (Ex $ Const 0) Int)

