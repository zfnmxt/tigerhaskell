{-# LANGUAGE DuplicateRecordFields #-}

module TypeChecker where

import AST
import Types
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Monad.State.Strict

data VEnvEntry = VarEntry {ty :: Ty}
               | FunEntry {argTys :: [Ty], retTy :: Ty}

type TExpr = ()
type TExprTy = (TExpr, Ty)
type VEnv = Map Id VEnvEntry
type TEnv = Map TypeId Ty
type Env  = (VEnv, TEnv)

type TError = String

baseTEnv = M.fromList [("int", Int), ("string", String)]
baseVEnv = M.fromList [ ("print",     FunEntry [String] Unit)
                      , ("flush",     FunEntry [] Unit)
                      , ("getchar",   FunEntry [] String)
                      , ("ord",       FunEntry [String] Int)
                      , ("chr",       FunEntry [Int] String)
                      , ("size",      FunEntry [String] Int)
                      , ("substring", FunEntry [String, Int, Int] String)
                      , ("concat",    FunEntry [String, String] String)
                      , ("not",       FunEntry [Int] Int)
                      , ("exit",      FunEntry [Int] Unit)
                      ]

transExpr :: Expr -> StateT Env (Either TError) TExprTy
transExpr expr =
  case expr of
    BExpr _ _ _ -> transBExpr expr
    _           -> lift $ Right "oops"

transBExpr :: Expr -> StateT Env (Either TError) TExprTy
transBExpr (BExpr op l r) = undefined
--  case op of
--    Add -> do
--     (_, lType) <- transExpr l
--     (_, rType) <- transExpr r









