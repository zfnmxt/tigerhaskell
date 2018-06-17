{-# LANGUAGE RecordWildCards #-}

module Translate where

import Tree
import Frame
import Temp
import STEnv
import Types

data TransExp = Ex TreeExp
              | Nx TreeStm
              | Cx (Label -> Label -> TreeStm)
              | NoExp

instance Show TransExp where
  show (Ex e) = "Ex " ++ show e
  show (Nx s) = "Nx " ++ show s
  show (Cx _) = "Cx <genStm>"
  show NoExp  = "NoExp"

instance Eq TransExp where
  (Ex e1) == (Ex e2) = e1 == e2
  (Nx s1) == (Nx s2) = s1 == s2
  NoExp == NoExp     = True
  _ == _             = False

seqMany :: [TreeStm] -> TreeStm
seqMany []     = error "oops"
seqMany (s:ss) = foldr Seq s ss

unEx :: TransExp -> STEnvT TreeExp
unEx NoExp       = return $ Const 0
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
  return $ ESeq (seqMany stms) (IReg r)

unNX :: TransExp -> STEnvT TreeStm
unNX (Ex e) = return $ StmExp e
unNX (Nx s) = return s
unNx (Cx genStm) = do
  e <- unEx (Cx genStm)
  unNX (Ex e)

unCX :: TransExp -> STEnvT (Label -> Label -> TreeStm)
uxCX (Const 0)   = return $ \_ f -> Jump (Name f) [f]
uxCX (Const 1)   = return $ \t _ -> Jump (Name t) [t]
unCX (Ex e)      = return $ \t f -> CJump NEqual (Const 0) e t f
unCX (Nx _)      = error "oops"
unCX (Cx genStm) = return genStm


-- get frame addr
followStatic :: VAccess -> Level -> TreeExp
followStatic vAccess@VAccess{..} cLevel
  | intLevel dLevel > intLevel cLevel        = error "oops"
  | _frameLabel dFrame == _frameLabel cFrame = IReg fP
  | otherwise                                =
    Mem $ BinOp Plus (followStatic vAccess cParent) parentFP
    where dLevel               = _accessLevel
          cFrame               = _levelFrame cLevel
          dFrame               = _levelFrame dLevel
          InFrame staticOffset = _frameStatic cFrame
          cParent              = _levelParent cLevel
          fP                   = _framePointer cFrame
          parentFP             = Mem $ BinOp Plus (IReg fP) (Const staticOffset)

-- uses frame addr and FAccess to yield TreeExp
fAccessToTree :: FAccess -> TreeExp -> TreeExp
fAccessToTree (InFrame offset) frameAddr = Mem $ BinOp Plus frameAddr (Const offset)
fAccessToTree (InReg reg) _              = IReg reg

vAccessToTree :: VAccess -> Level -> TreeExp
vAccessToTree vAccess@VAccess{..} cLevel =
  let frameAddr = followStatic vAccess cLevel
  in fAccessToTree _accessLoc frameAddr

simpleVar :: VAccess -> Level -> STEnvT TransExp
simpleVar vAccess cLevel = return . Ex $ vAccessToTree vAccess cLevel

iExpr :: Int -> STEnvT TransExp
iExpr x = return $ Ex (Const x)

fCall :: VEnvEntry -> Level -> [TransExp] -> STEnvT TransExp
fCall f@FunEntry{..} cLevel transArgs = do
  treeArgs <- mapM unEx transArgs
  let staticLink = getStaticLink f cLevel
      fLabel     = Name _funEntryLabel
      treeArgs'  = staticLink:treeArgs
      transCall  = Call fLabel treeArgs'
  case _funEntryRetTy of
    Unit -> return $ Nx $ StmExp transCall
    _    -> return $ Ex transCall

levelLabel :: Level -> Label
levelLabel Level{..} = _frameLabel _levelFrame

getStaticLink :: VEnvEntry -> Level -> TreeExp
getStaticLink f cLevel
  | levelLabel fLevel == levelLabel cLevel =
    Mem (Const slLoc)
  | otherwise = getStaticLink f cParent
  where fLevel         = _funEntryLevel f
        fFrame         = _levelFrame fLevel
        InFrame slLoc  = _frameStatic fFrame
        cParent        = _levelParent cLevel


