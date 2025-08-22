{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Translate
  ( Level (..),
    Access (..),
    outermost,
    newLevel,
    formals,
    allocLocal,
    Escape,
    Exp,
    simpleVar,
    fieldAccess,
    subscriptAccess,
    constant,
    nil,
    opExp,
    string,
    call,
    malloc,
    record,
    seqExp,
    assign,
  )
where

import AST (Oper (..))
import Data.Proxy
import Frame
  ( Escape,
    Frame,
  )
import Frame qualified
import Symbol
import Temp qualified
import Tree qualified as T

data Level f = Level
  { levelNum :: Integer,
    levelFrame :: f,
    levelParent :: Maybe (Level f)
  }

instance Eq (Level f) where
  (Level l1 _ _) == (Level l2 _ _) = l1 == l2

deriving instance (Show f) => Show (Level f)

deriving instance (Ord f) => Ord (Level f)

data Access f = Access
  { accessLevel :: Level f,
    accessFrame :: Frame.Access f
  }

deriving instance (Show f, Show (Frame.Access f)) => Show (Access f)

deriving instance (Eq f, Eq (Frame.Access f)) => Eq (Access f)

deriving instance (Ord f, Ord (Frame.Access f)) => Ord (Access f)

outermost :: (Frame f) => Level f
outermost =
  Level
    { levelNum = 0,
      levelFrame = undefined,
      levelParent = Nothing
    }

newLevel :: (Frame f) => Level f -> Temp.Label -> [Escape] -> Level f
newLevel parent name formals =
  let frame = Frame.newFrame name (True : formals)
   in Level
        { levelNum = levelNum parent + 1,
          levelFrame = frame,
          levelParent = Just parent
        }

formals :: (Frame f) => Level f -> [Access f]
formals lvl =
  map (Access lvl) $ Frame.formals $ levelFrame lvl

allocLocal :: (Frame f) => Level f -> Escape -> Access f
allocLocal lvl escape =
  Access
    { accessLevel = lvl,
      accessFrame = Frame.allocLocal (levelFrame lvl) escape
    }

data Exp
  = Ex T.Exp
  | Nx T.Stm
  | Cx (Temp.Label -> Temp.Label -> T.Stm)

unEx :: (MonadSym m) => Exp -> m T.Exp
unEx (Ex e) = pure e
unEx (Nx stm) = pure $ T.ESeq stm $ T.Const 0
unEx (Cx mkStm) = do
  r <- Temp.newTemp
  t <- Temp.newLabel
  f <- Temp.newLabel
  pure $
    T.ESeq
      ( T.seq
          [ T.Move (T.Temp r) 1,
            mkStm t f,
            T.Label f,
            T.Move (T.Temp r) 0,
            T.Label t
          ]
      )
      (T.Temp r)

unNx :: (MonadSym m) => Exp -> m T.Stm
unNx (Ex e) = pure $ T.Exp e
unNx (Nx stm) = pure stm
unNx (Cx mkStm) = do
  done <- Temp.newLabel
  pure $
    T.seq
      [ mkStm done done,
        T.Label done
      ]

unCx :: (MonadSym m) => Exp -> m (Temp.Label -> Temp.Label -> T.Stm)
unCx (Ex 0) = pure $ \_ f -> T.Jump (T.Name f) [f]
unCx (Ex 1) = pure $ \t _ -> T.Jump (T.Name t) [t]
unCx (Ex e) = pure $ T.CJump T.NE e 0
unCx (Nx _) = error "unCx: Nx isn't supported."
unCx (Cx mkStm) = pure mkStm

simpleVar :: forall frame. (Frame frame) => Access frame -> Level frame -> Exp
simpleVar access lvl =
  Ex $ Frame.exp (accessFrame access) (stackFrame access lvl)
  where
    stackFrame access' lvl'
      | accessLevel access' == lvl' = T.Temp (Frame.fP (Proxy @frame))
      | otherwise =
          case levelParent lvl' of
            Nothing -> error "stackFrame: no parent."
            Just parent ->
              stackFrame access' parent
                + T.Mem (Frame.staticLink (levelFrame lvl'))

fieldAccess :: (MonadSym m) => Exp -> Integer -> Integer -> m Exp
fieldAccess record field size = do
  record' <- unEx record
  pure $ Ex $ record' + fromInteger field * fromInteger size

subscriptAccess :: (MonadSym m) => Exp -> Exp -> Integer -> m Exp
subscriptAccess array offset size = do
  array' <- unEx array
  offset' <- unEx offset
  pure $ Ex $ array' + offset' * fromInteger size

constant :: Integer -> Exp
constant = Ex . T.Const

nil :: Exp
nil = Ex $ T.Const 0

opExp :: (MonadSym m) => Exp -> Oper -> Exp -> m Exp
opExp l op r = do
  l' <- unEx l
  r' <- unEx r
  case transOp of
    Left op' -> pure $ Ex $ T.BinOp op' l' r'
    Right op' -> pure $ Cx $ T.CJump op' l' r'
  where
    transOp = case op of
      PlusOp -> Left T.Plus
      MinusOp -> Left T.Minus
      TimesOp -> Left T.Mul
      DivideOp -> Left T.Div
      EqOp -> Right T.Eq
      NeqOp -> Right T.NE
      LtOp -> Right T.LT
      LeOp -> Right T.LE
      GtOp -> Right T.GT
      GeOp -> Right T.GE

string :: (MonadSym m) => String -> m Exp
string s = do
  l <- Temp.newLabel
  -- TODO: Fragments
  pure $ Ex $ T.Name l

call :: forall frame m. (Frame frame, MonadSym m) => Level frame -> Level frame -> Temp.Label -> [Exp] -> m Exp
call lvl f_lvl l args = do
  args' <- mapM unEx args
  let sl = staticLink lvl
  pure $ Ex $ T.Call (T.Name l) (sl : args')
  where
    staticLink lvl'
      | f_lvl == lvl' = T.Temp (Frame.fP (Proxy @frame))
      | otherwise =
          case levelParent lvl' of
            Nothing -> error "stackLink: no parent."
            Just parent ->
              staticLink parent
                + T.Mem (Frame.staticLink (levelFrame lvl'))

malloc :: forall frame m. (Frame frame, MonadSym m) => Exp -> m Exp
malloc size = do
  size' <- unEx size
  pure $ Ex $ Frame.externalCall (Proxy @frame) "malloc" [size']

record :: forall frame m. (Frame frame, MonadSym m) => [Exp] -> m Exp
record fields = do
  fields' <- mapM unNx fields
  r <- unEx =<< (malloc @frame) (Ex $ fromInteger $ fromIntegral $ length fields)
  pure $ Ex $ T.ESeq (T.seq fields') r

seqExp :: forall m. (MonadSym m) => [Exp] -> m Exp
seqExp = fmap Ex . seqExp'
  where
    seqExp' :: [Exp] -> m T.Exp
    seqExp' [] = pure 0
    seqExp' [e] = unEx e
    seqExp' (e : es) = do
      e' <- unNx e
      T.ESeq e' <$> seqExp' es
