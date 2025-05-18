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
  )
where

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
            Nothing -> error "simpleVar: no parent."
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
