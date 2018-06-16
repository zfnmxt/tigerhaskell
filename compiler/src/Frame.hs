{-# LANGUAGE RecordWildCards #-}

module Frame where

import Temp (Temp, Label)

_VARSIZE = 4

data Access = InFrame Int | InReg Temp
  deriving (Eq, Show)

data Frame = Frame { _frameLabel     :: Label
                   , _frameEscapes   :: [Bool]
                   , _frameFormals   :: [Access]
                   , _frameLocals    :: [Access]
                   , _frameLocalsLen :: Int
                   } deriving (Eq, Show)

lVarOffset :: Int -> Int
lVarOffset locals = locals * (- _VARSIZE)

newFrame :: Label -> [Bool] -> Frame
newFrame name args =
  Frame { _frameLabel     = name
        , _frameEscapes   = args
        , _frameLocals    = []
        , _frameLocalsLen = 0
        }

allocMem :: Frame -> (Frame, Access)
allocMem frame@Frame{..} = (frame', access)
  where access = InFrame $ lVarOffset (_frameLocalsLen + 1)
        frame' = frame { _frameLocals    = _frameLocals ++ [access]
                       , _frameLocalsLen = _frameLocalsLen + 1
                       }

allocReg :: Frame -> Temp -> (Frame, Access)
allocReg frame@Frame{..} temp = (frame', access)
  where access = InReg temp
        frame' = frame { _frameLocals    = _frameLocals ++ [access]
                       , _frameLocalsLen = _frameLocalsLen + 1
                       }




