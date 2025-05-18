{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Translate
  ( Level (..),
    Access (..),
    outermost,
    newLevel,
    formals,
    allocLocal,
    Escape,
  )
where

import Frame
  ( Escape,
    Frame,
  )
import Frame qualified
import Symbol
import Temp qualified
import Tree qualified as T

data Level f = Level
  { levelNum :: Int,
    levelFrame :: f
  }

deriving instance (Show f) => Show (Level f)

deriving instance (Eq f) => Eq (Level f)

deriving instance (Ord f) => Ord (Level f)

data Access f = Access
  { accessLevel :: Level f,
    accessFrameAccess :: Frame.Access f
  }

deriving instance (Show f, Show (Frame.Access f)) => Show (Access f)

deriving instance (Eq f, Eq (Frame.Access f)) => Eq (Access f)

deriving instance (Ord f, Ord (Frame.Access f)) => Ord (Access f)

outermost :: (Frame f) => Level f
outermost =
  Level
    { levelNum = 0,
      levelFrame = undefined
    }

newLevel :: (Frame f) => Level f -> Temp.Label -> [Escape] -> Level f
newLevel parent name formals =
  let frame = Frame.newFrame name (True : formals)
   in Level
        { levelNum = levelNum parent + 1,
          levelFrame = frame
        }

formals :: (Frame f) => Level f -> [Access f]
formals lvl =
  map (Access lvl) $ Frame.formals $ levelFrame lvl

allocLocal :: (Frame f) => Level f -> Escape -> Access f
allocLocal lvl escape =
  Access
    { accessLevel = lvl,
      accessFrameAccess = Frame.allocLocal (levelFrame lvl) escape
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
unCx (Ex e) = pure $ T.CJump T.Eq e 1
unCx (Nx _) = error "unCx: Nx isn't supported."
unCx (Cx mkStm) = pure mkStm
