{-# LANGUAGE RecordWildCards #-}

module Translate where

import qualified Tree  as T
import qualified Frame as F
import Temp
import STEnv

data Exp = Ex T.Exp
         | Nx T.Stm
         | Cx (Label -> Label -> T.Stm)

seqMany :: [T.Stm] -> T.Stm
seqMany []     = error "oops"
seqMany (s:ss) = foldr Seq s ss

unEx :: Exp -> STEnvT T.Exp
unEx (Ex e)      = return $ e
unEx (Nx s)      = return $ ESeq s (Const 0)
unEx (Cx genStm) = do
  r <- mkTemp
  t <- mkLabel
  f <- mkLabel
  let stms = [ Move (IReg r) (Const 1)
             , genStm t f
             , StmLabel f
             , Move (IReg r) (Const 0)
             , StmLabel t
             ]
  return $ ESeq (seqMany stms) (TempE r)

unNX :: Exp -> STEnvT T.Stm
unNX (Ex e) = return $ StmExp e
unNX (Nx s) = return s
unNx (Cx genStm) = do
  e <- unEx (Cx genStm)
  unNX (Ex e)

unCX :: Exp -> STEnvT (Label -> Label -> T.Stm)
uxCX (Const 0)   = return $ \_ f -> Jump (StmLabel f) [f]
uxCX (Const 1)   = return $ \t _ -> Jump (StmLabel t) [t]
unCX (Ex e)      = return $ \t f -> CJump NEqual (Const 0) e t f
unCX (Nx _)      = error "oops"
unCX (Cx genStm) = return genStm

simpleVar :: Access -> Level -> STEnvT Exp
simpleVar Access{..} level =
  case _accessLoc of
    InFrame offset ->
      let memExp = BinOp Plus (RTemp RBP) (Const offset)
      in return $ Mem memExp
    InReg reg      ->
      return IReg reg


