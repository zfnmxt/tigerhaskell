{-# LANGUAGE MultiParamTypeClasses #-}

module Translate
  ( Level (..),
    Access (..),
    outermost,
    newLevel,
    formals,
    allocLocal,
  )
where

import Frame
  ( Escape,
    Frame,
  )
import Frame qualified
import Symbol
import Temp qualified

data Level f = Level
  { levelNum :: Int,
    levelFrame :: f
  }
  deriving (Show, Eq, Ord)

data Access f = Access
  { accessLevel :: Level f,
    accessFrameAccess :: Frame.Access f
  }

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
