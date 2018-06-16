{-# LANGUAGE RecordWildCards #-}

module Translate where

import qualified Frame as F
import Temp

data Level = Outermost | Level { _levelParent  :: Level
                               , _levelFrame   :: F.Frame
                               } deriving (Eq, Show)

data Access = Access { _accessLevel   :: Level
                     , _accessFAccess :: F.Access
                     }
              deriving (Eq, Show)

newLevel :: Level -> Label -> [Bool] -> Level
newLevel parent name args = Level parent frame
  where frame = F.newFrame name (True:args)


allocLocal :: Level -> Temp -> Bool -> (Level, Access)
allocLocal Outermost _ _ = error "oops"
allocLocal level@Level{..} temp esc = (level', Access level' fAccess')
  where level' = level { _levelFrame = frame'}
        (frame', fAccess')
         | esc       = F.allocMem _levelFrame
         | otherwise = F.allocReg _levelFrame temp






