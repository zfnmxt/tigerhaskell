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

_STRINGEQUAL = "stringEqual"
_STRINGLT    = "stringLT"
_STRINGGT    = "stringGT"
_ALLOC       = "alloc"
_INITARRAY   = "initArray"
_NIL         = "nil"


--------------------------------------------------------------------------------
-- TransExp
--------------------------------------------------------------------------------

data TransExp = Ex TreeExp
              | Nx TreeStm
              | Cx (Label -> Label -> TreeStm)

instance Show TransExp where
  show (Ex e) = "Ex " ++ show e
  show (Nx s) = "Nx " ++ show s
  show (Cx _) = "Cx <genStm>"

instance Eq TransExp where
  (Ex e1) == (Ex e2) = e1 == e2
  (Nx s1) == (Nx s2) = s1 == s2
  _ == _             = False

seqMany :: [TreeStm] -> TreeStm
seqMany []     = StmExp (Const 0)
seqMany (s:ss) = foldr (>>>) s ss

unEx :: TransExp -> STEnvT TreeExp
unEx (Ex e)      = return $ e
unEx (Nx s)      = return $ ESeq s (Const 0)
unEx (Cx genStm) = do
  r <- mkTemp
  t <- mkLabel
  f <- mkLabel
  let stms = seqMany [ Move (IReg r) (Const 1)
                     , genStm t f
                     , StmLabel f
                     , Move (IReg r) (Const 0)
                     , StmLabel t
                     ]
  return $ stms >>$ IReg r

unNx :: TransExp -> STEnvT TreeStm
unNx (Ex e) = return $ StmExp e
unNx (Nx s) = return s
unNx (Cx genStm) = do
  e <- unEx (Cx genStm)
  unNx (Ex e)

unCx :: TransExp -> STEnvT (Label -> Label -> TreeStm)
uxCx (Const 0)   = return $ \_ f -> Jump (Name f) [f]
uxCx (Const 1)   = return $ \t _ -> Jump (Name t) [t]
unCx (Ex e)      = return $ \t f -> CJump NEqual (Const 0) e t f
unCx (Nx _)      = error "oops"
unCx (Cx genStm) = return genStm


--------------------------------------------------------------------------------
-- Translation
--------------------------------------------------------------------------------
indexOffset :: Int -> TreeExp -> TreeExp
indexOffset offset addr = Mem (BinOp Plus (Const offset) addr)

-- Get address of var in memory
varIndex :: VAccess -> Level -> TreeExp
varIndex vAccess@VAccess{..} cLevel
  | intLevel _accessLevel > intLevel cLevel = error "oops"
  | otherwise                               =
    buildIndex cLevel (IReg _R_FP)
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

tIntComp :: AST.BOp -> TransExp -> TransExp -> STEnvT TransExp
tIntComp astOP lTrans rTrans = do
  l <- unEx lTrans
  r <- unEx rTrans
  let op = case astOP of
            AST.Gt     -> Gt
            AST.Lt     -> Lt
            AST.GTE    -> GTE
            AST.LTE    -> LTE
            AST.Equal  -> Equal
            AST.NEqual -> NEqual
  return $ Cx $ \tLabel fLabel ->
                  CJump op l r tLabel fLabel

tStringEq :: TransExp -> TransExp -> STEnvT TransExp
tStringEq  lTrans rTrans = do
  l <- unEx lTrans
  r <- unEx rTrans
  let strEqualTree = externCall _STRINGEQUAL [l, r]
  return $ Ex $ strEqualTree

tStringNEq :: TransExp -> TransExp -> STEnvT TransExp
tStringNEq lTrans rTrans = do
  Ex strEqTree <- tStringEq lTrans rTrans
  return $ Cx $ \tLabel fLabel ->
                      CJump Equal strEqTree (Const 0) tLabel fLabel

tStringEqNEq :: AST.BOp -> TransExp -> TransExp -> STEnvT TransExp
tStringEqNEq (AST.Equal)  = tStringEq
tStringEqNEq (AST.NEqual) = tStringNEq

tStringLt :: TransExp -> TransExp -> STEnvT TransExp
tStringLt lTrans rTrans = do
  l <- unEx lTrans
  r <- unEx rTrans
  let strLtTree = externCall _STRINGLT [l, r]
  return $ Ex $ strLtTree

tStringLTE :: TransExp -> TransExp -> STEnvT TransExp
tStringLTE lTrans rTrans = do
  strEqTree <- tStringEq lTrans rTrans
  strLtTree <- tStringLt lTrans rTrans
  tIFE strLtTree (Ex (Const 1)) strEqTree

tStringGt :: TransExp -> TransExp -> STEnvT TransExp
tStringGt lTrans rTrans = do
  l <- unEx lTrans
  r <- unEx rTrans
  let strGtTree = externCall _STRINGGT [l, r]
  return $ Ex $ strGtTree

tStringGTE :: TransExp -> TransExp -> STEnvT TransExp
tStringGTE lTrans rTrans = do
  strEqTree <- tStringEq lTrans rTrans
  strGtTree <- tStringGt lTrans rTrans
  tIFE strGtTree (Ex (Const 1)) strEqTree

tStringComp :: AST.BOp -> TransExp -> TransExp -> STEnvT TransExp
tStringComp AST.Gt   = tStringGt
tStringComp AST.Lt   = tStringLt
tStringComp AST.GTE  = tStringGTE
tStringComp AST.LTE  = tStringLTE

tNeg :: TransExp -> STEnvT TransExp
tNeg eTrans = do
  e <- unEx eTrans
  return $ Ex $ BinOp Sub (Const 0) e

tIFE :: TransExp -> TransExp -> TransExp -> STEnvT TransExp
tIFE condTrans (Nx thenStm) (Nx elseStm) = do
  condGenStm <- unCx condTrans
  tLabel <- mkLabel
  fLabel <- mkLabel
  zLabel <- mkLabel
  return $ Nx $ seqMany [ condGenStm tLabel fLabel
                        , StmLabel tLabel
                        , thenStm
                        , Jump (Name zLabel) [zLabel]
                        , StmLabel fLabel
                        , elseStm
                        , StmLabel zLabel
                        ]
tIFE condTrans (Cx thenCond) (Cx elseCond) = do -- (e1 & e2 | e3)
  condGenStm <- unCx condTrans
  yLabel <- mkLabel
  zLabel <- mkLabel
  return $ Cx $ \tLabel fLabel ->
    seqMany [ condGenStm zLabel yLabel
            , StmLabel zLabel
            , thenCond tLabel yLabel
            , StmLabel yLabel
            , elseCond tLabel fLabel
             ]
tIFE condTrans (Cx thenCond) elseTrans = do -- (e1 & e2)
  condGenStm <- unCx condTrans
  yLabel  <- mkLabel
  zLabel  <- mkLabel
  elseExp <- unEx elseTrans
  return $ Cx $ \tLabel fLabel ->
    seqMany [ condGenStm zLabel yLabel
            , StmLabel zLabel
            , thenCond tLabel fLabel
            , StmLabel yLabel
            , CJump NEqual (Const 0) elseExp tLabel fLabel
            ]
tIFE condTrans thenTrans (Cx elseCond) = do-- (e1 | e3)
  condGenStm <- unCx condTrans
  yLabel  <- mkLabel
  zLabel  <- mkLabel
  thenExp <- unEx thenTrans
  return $ Cx $ \tLabel fLabel ->
    seqMany [ condGenStm yLabel zLabel
            , StmLabel yLabel
            , CJump NEqual (Const 0) thenExp tLabel fLabel
            , StmLabel zLabel
            , elseCond tLabel fLabel
            ]
tIFE condTrans thenTrans elseTrans = do
  condGenStm <- unCx condTrans
  thenExp    <- unEx thenTrans
  elseExp    <- unEx elseTrans
  r          <- mkTemp
  tLabel     <- mkLabel
  fLabel     <- mkLabel
  return $ Ex $
    (seqMany [ Move (IReg r) thenExp
             , condGenStm tLabel fLabel
             , StmLabel fLabel
             , Move (IReg r) elseExp
             , StmLabel tLabel
            ])
    >>$ IReg r

tIF :: TransExp -> TransExp -> STEnvT TransExp
tIF condTrans thenTrans  = do
  condGenStm <- unCx condTrans
  thenStm    <- unNx thenTrans
  tLabel <- mkLabel
  fLabel <- mkLabel
  return $ Nx $ seqMany [ condGenStm tLabel fLabel
                        , StmLabel tLabel
                        , thenStm
                        , StmLabel fLabel
                        ]

tRecord :: [TransExp] -> STEnvT TransExp
tRecord tFields = do
  let n       = length tFields
      recordP = externCall _ALLOC [Const n]
  exps        <- mapM unEx tFields
  let moves   = zipWith (f recordP) [0..(n-1)] exps
  return $ Ex $ seqMany moves >>$ recordP
    where f recordP i e =
            let loc = BinOp Plus recordP (BinOp Mul (Const i) (Const _WORDSIZE))
            in Move loc e

tArray :: TransExp -> TransExp -> STEnvT TransExp
tArray nTrans vTrans = do
  n <- unEx nTrans
  v <- unEx vTrans
  return $ Ex $  externCall _INITARRAY [n, v]

tWhileCond :: TransExp -> STEnvT (TransExp -> STEnvT TransExp)
tWhileCond condTrans = do
  condGenStm <- unCx condTrans
  testLabel  <- mkLabel
  doneLabel  <- mkLabel
  tLabel     <- mkLabel
  pushBreak doneLabel
  return $ \bodyTrans -> do
    bodyStm <- unNx bodyTrans
    return $ Nx $ seqMany [ StmLabel testLabel
                          , condGenStm tLabel doneLabel
                          , StmLabel tLabel
                          , bodyStm
                          , Jump (Name testLabel) [testLabel]
                          , StmLabel doneLabel
                          ]

tFor :: TransExp -> TransExp -> TransExp -> TransExp -> STEnvT (TransExp -> STEnvT TransExp)
tFor vInitTrans vTrans minTrans maxTrans = do
  v     <- unEx vTrans
  min   <- unEx minTrans
  vInit <- unNx vInitTrans
  max   <- unEx maxTrans
  done  <- mkLabel
  test  <- mkLabel
  loop  <- mkLabel
  lt    <- mkLabel
  pushBreak done
  return $ \bodyTrans -> do
    bodyStm <- unNx bodyTrans
    return $ Nx $ seqMany [ vInit
                          , StmLabel test
                          , CJump LTE v max loop done
                          , StmLabel loop
                          , bodyStm
                          , CJump Lt v max lt done
                          , StmLabel lt
                          , Move v (BinOp Plus v (Const 1))
                          , Jump (Name test) [test]
                          , StmLabel done
                          ]

tAssign :: TransExp -> TransExp -> STEnvT TransExp
tAssign vTrans eTrans = do
  v <- unEx vTrans
  e <- unEx eTrans
  return $ Nx $ Move v e

tVarDef :: VAccess -> Level -> TransExp -> STEnvT TransExp
tVarDef vAccess level eTrans = do
  vTrans <- simpleVar vAccess level
  tAssign vTrans eTrans

tFunDef :: TransExp -> Level -> STEnvT TransExp
tFunDef bodyTrans level = do
  ret <- unEx bodyTrans
  let frame = _levelFrame level
  let moveRet = Move (IReg _R_RET) ret
  let body = procEntryExit1 frame moveRet
  insertFrag $ Proc {_procBody = body, _procFrame = frame}
  return $ Nx $ body

procEntryExit :: Level -> TransExp -> STEnvT ()
procEntryExit level exp = undefined

tString :: String -> STEnvT TransExp
tString s = do
  label <- mkLabel
  insertFrag $ FString {_fStringLabel = label, _fStringS = s}
  return $ Ex $ Name label

tNil :: STEnvT TransExp
tNil = return $ Ex $ Mem (Name (NamedLabel "_NIL"))

tBreak :: STEnvT TransExp
tBreak = do
  label <- popBreak
  return $ Nx $ Jump (Name label) [label]

tUnit :: STEnvT TransExp
tUnit = return $ Nx $ StmExp (Const 0)

tArrayRecEqNeq :: AST.BOp -> TransExp -> TransExp -> STEnvT TransExp
tArrayRecEqNeq astBOp leftTrans rightTrans = do
  left   <- unEx leftTrans
  right  <- unEx rightTrans
  let op = case astBOp of {AST.Equal -> Equal; AST.NEqual -> NEqual}
  return $ Cx $ \t f -> CJump op left right t f

tLet :: [TransExp] -> [TransExp] -> STEnvT TransExp
tLet _ [] = return $ Nx $ StmExp (Const 0)
tLet tDecs transExps = do
  stms     <- mapM unNx tDecs
  bodyStms <- mapM unNx (init transExps)
  retExp   <- unEx (last transExps)
  return $ Ex (seqMany stms >>> seqMany bodyStms >>$ retExp)
