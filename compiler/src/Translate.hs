{-# LANGUAGE RecordWildCards #-}

module Translate where

import qualified AST 
import           AST (Id)
import           Data.List (elemIndex)
import           Frame
import           STEnv
import           Temp
import           Tree
import           Types

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
uxCX (Const 0) = return $ \_ f -> Jump (Name f) [f]
uxCX (Const 1) = return $ \t _ -> Jump (Name t) [t]
unCX (Ex e)      = return $ \t f -> CJump NEqual (Const 0) e t f
unCX (Nx _)      = error "oops"
unCX (Cx genStm) = return genStm


-- Get FP of currnet level
levelFP :: Level -> Temp
levelFP Level{..} = _framePointer _levelFrame

-- Get the static link of the current level
levelSL :: Level -> TreeExp
levelSL level = Mem (BinOp Plus (Const sLOffset) (IReg fP))
  where cFrame           = _levelFrame level
        InFrame sLOffset = _frameStatic cFrame
        fP               = _framePointer cFrame

-- Get the static link of the level's parent
pLevelSL :: Level -> TreeExp
pLevelSL level = Mem (BinOp Plus (Const pSLOffset) (levelSL level))
  where pFrame            = _levelFrame (_levelParent level)
        InFrame pSLOffset = _frameStatic pFrame

levelOffset :: Int -> Level -> TreeExp
levelOffset offset level = Mem (BinOp Plus (Const offset) (IReg fP))
  where cFrame           = _levelFrame level
        fP               = _framePointer cFrame

levelLabel :: Level -> Label
levelLabel Level{..} = _frameLabel _levelFrame

indexOffset :: Int -> TreeExp -> TreeExp
indexOffset offset addr = Mem (BinOp Plus (Const offset) addr)

-- Get address of var in memory
varIndex :: VAccess -> Level -> TreeExp
varIndex vAccess@VAccess{..} cLevel
  | intLevel _accessLevel > intLevel cLevel = error "oops"
  | otherwise                               =
    buildIndex cLevel (IReg (levelFP cLevel))
    where InFrame vOffset = _accessLoc
          buildIndex cLevel tree
           | levelLabel cLevel == levelLabel _accessLevel =
               Mem (BinOp Plus (Const vOffset) tree)
           | otherwise =
               let pLevel          = _levelParent cLevel in
               let InFrame sOffset = _frameStatic $ _levelFrame cLevel in
                 buildIndex pLevel (Mem (BinOp Plus (Const sOffset) tree))

-- uses frame addr and FAccess to yield TreeExp
fAccessToTree :: FAccess -> TreeExp -> TreeExp
fAccessToTree (InFrame offset) frameAddr = Mem $ BinOp Plus frameAddr (Const offset)
fAccessToTree (InReg reg) _              = IReg reg

simpleVar :: VAccess -> Level -> STEnvT TransExp
simpleVar vAccess cLevel = return . Ex $ varIndex vAccess cLevel

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

getStaticLink :: VEnvEntry -> Level -> TreeExp
getStaticLink f cLevel
  | levelLabel fLevel == levelLabel cLevel =
    Mem (Const slLoc)
  | otherwise = getStaticLink f cParent
  where fLevel         = _funEntryLevel f
        fFrame         = _levelFrame fLevel
        InFrame slLoc  = _frameStatic fFrame
        cParent        = _levelParent cLevel

arrayVar :: TransExp -> TransExp -> STEnvT TransExp
arrayVar aTrans iTrans = do
  i <- unEx iTrans
  a <- unEx aTrans
  let elemOffset = BinOp Mul i (Const _WORDSIZE)
  return $ Ex $ Mem $ BinOp Plus a elemOffset

recordVar :: Ty -> TransExp -> Id -> STEnvT TransExp
recordVar ty rTrans f = do
  r <- unEx rTrans
  let i = getFieldPos f ty
      fieldOffset = BinOp Mul (Const i) (Const _WORDSIZE)
  return $ Ex $ Mem $ BinOp Plus r fieldOffset
  where getFieldPos f (Record _ (Just fs)) =
          case elemIndex f (map fst fs) of
            Just i  -> i
            Nothing -> error "oops"

tArith :: AST.BOp -> TransExp -> TransExp -> STEnvT TransExp
tArith astOP lTrans rTrans = do
  l <- unEx lTrans
  r <- unEx rTrans
  let op = case astOP of
            AST.Add  -> Plus
            AST.Sub  -> Sub
            AST.Mult -> Mul
            AST.Div  -> Div
  return $ Ex $ BinOp op l r

tNeg :: TransExp -> STEnvT TransExp
tNeg eTrans = do
  e <- unEx eTrans
  return $ Ex $ BinOp Sub (Const 0) e

