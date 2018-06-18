{-# LANGUAGE RecordWildCards #-}

module Spec.Translate where

import Test.Hspec
import Test.QuickCheck
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, execStateT)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Spec.AppelTigFiles
import Spec.Parser (testParseE)
import Spec.Semant

import Parser
import Semant
import AST
import Types
import Translate
import Frame
import STEnv
import qualified Tree as T
import Tree
import Temp
import Registers

evalStateT' m = evalStateT m initEnv
execStateT' m = execStateT m initEnv

parseAndTran :: String -> Either TError TExpr
parseAndTran s = case testParseE s of
  Left _ -> Left "oops"
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
  it "simple var" $ do
     let vDec    = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     let letExpr = Let [vDec] [VExpr (SimpleVar "x")]
     let tExpr   = Ex $ Mem $ BinOp T.Plus (IReg (RTemp RBX)) (Const (-4))
     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr tExpr Int)
  it "two simple vars" $ do
     let vDec1   = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     let vDec2   = VarDec $ VarDef "y" (Just "int") (IExpr 1)
     let letExpr = Let [vDec1, vDec2] [ExprSeq [VExpr (SimpleVar "x"), VExpr (SimpleVar "y")]]
     let tExpr   = Ex $ Mem $ BinOp T.Plus (IReg (RTemp RBX)) (Const (-8))
     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr tExpr Int)
  it "function call" $ do
     let fDec    = FunDec [FunDef "f" [Field "y" "int" True] (Just "int") (IExpr 1)]
     let letExpr = Let [fDec] [FCall "f" [IExpr 1]]
     let tExpr   = Ex $ Mem $ BinOp T.Plus (IReg (RTemp RBX)) (Const (-8))
     evalStateT' (transExpr letExpr) `shouldBe`
       Right (TExpr (Ex (Call (Name (Label 11)) [Mem (Const 0),Const 1])) Int)
  it "nested simple var" $ do
     let vDec  = VarDec $ VarDef "x" (Just "int") (IExpr 5)
     let fDec  = FunDec [FunDef "f" [Field "y" "int" True] (Just "int") (VExpr (SimpleVar "x"))]
     let letExpr = Let [vDec, fDec] [FCall "f" [IExpr 1]]
     let tExpr   = Ex $ Mem $ BinOp T.Plus (IReg (RTemp RBX)) (Const (-8))
     evalStateT' (transExpr letExpr) `shouldBe` Right (TExpr tExpr Int)



