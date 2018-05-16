{-# LANGUAGE DuplicateRecordFields #-}

module TypeChecker where

import AST
import Types
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Control.Monad.State.Strict as S
import Control.Monad.State.Strict (StateT, lift, runStateT)


data VEnvEntry = VarEntry {ty :: Ty}
               | FunEntry {argTys :: [Ty], retTy :: Ty}
               deriving (Show, Eq)

type TExpr = ()
type TExprTy = (TExpr, Ty)
type VEnv = Map Id VEnvEntry
type TEnv = Map TypeId Ty
type Env  = (VEnv, TEnv)

type TError = String
type CheckerState = StateT Env (Either TError) TExprTy

genError :: Expr -> String -> TError
genError expr s = "Error in expr:\n" ++ show expr ++ "\n with error msg: " ++ s
                          --
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

initEnv = (baseVEnv, baseTEnv)

transExpr :: Expr -> CheckerState
transExpr expr =
  case expr of
    BExpr _ _ _ -> transBExpr expr
    IExpr _     -> lift . Right $ ((), Int)
    SExpr _     -> lift . Right $ ((), String)
    NExpr _     -> transNExpr expr
    VExpr _     -> transVExpr expr

transBExpr :: Expr -> CheckerState
transBExpr expr@(BExpr op l r)
  | op `elem` [Add, Sub, Mult, Div, And, Or] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int) -> lift . Right $ ((), Int)
       _          -> lift . Left $ genError expr "int required"
  | op `elem` [Equal, NEqual, Gt, Lt, GTE, LTE] = do
     (_, lType) <- transExpr l
     (_, rType) <- transExpr r
     case (lType, rType) of
       (Int, Int)       -> lift . Right $ ((), Int)
       (String, String) -> lift . Right $ ((), Int)
       _                -> lift . Left $ genError expr "ints or strings required"

transNExpr :: Expr -> CheckerState
transNExpr nExpr@(NExpr expr) = do
  (_, eType) <- transExpr expr
  case eType of
    Int -> lift . Right $ ((), Int)
    _   -> lift . Left $ genError nExpr "int required"

transVExpr :: Expr -> CheckerState
transVExpr vExpr@(VExpr var) = do
  case var of
    SimpleVar x -> do
      (vEnv, _tEnv) <- S.get
      case M.lookup x vEnv of
        Just (VarEntry vType) -> lift . Right $ ((), vType)
        Just (FunEntry _ _  ) -> lift . Left $ genError vExpr "function with same name exists"
        _                     -> lift . Left $ genError vExpr "undefined var"












