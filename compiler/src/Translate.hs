{-# LANGUAGE RecordWildCards #-}

module Translate where

import Tree hiding (Exp)
import qualified Tree
import Frame
import Temp
import STEnv

data Exp = Ex Tree.Exp
         | Nx Stm
         | Cx (Label -> Label -> Stm)

seqMany :: [Stm] -> Stm
seqMany []     = error "oops"
seqMany (s:ss) = foldr Seq s ss

unEx :: Exp -> STEnvT Tree.Exp
unEx (Ex e)      = return $ e
unEx (Nx s)      = return $ ESeq s (Const 0)
unEx (Cx genStm) = do
  r <- mkTemp
  t <- mkLabel
  f <- mkLabel
  let stms = [ Move (TempE r) (Const 1)
             , genStm t f
             , StmLabel f
             , Move (TempE r) (Const 0)
             , StmLabel t
             ]
  return $ ESeq (seqMany stms) (TempE r)

unNX :: Exp -> STEnvT Tree.Stm
unNX (Ex e) = return $ StmExp e
unNX (Nx s) = return s
unNx (Cx genStm) = do
  e <- unEx (Cx genStm)
  unNX (Ex e)

unCX :: Exp -> STEnvT (Label -> Label -> Stm)
unCX (Ex e)      = return $ \t f -> StmExp e
unCX (Nx s)      = return $ \t f -> s
unCX (Cx genStm) = return genStm


