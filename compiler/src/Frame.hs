{-# LANGUAGE RecordWildCards #-}

module Frame where

import Temp
import Registers
import Tree

_WORDSIZE = 4

data FAccess = InFrame Int | InReg Temp
  deriving (Eq, Show)

data Frame = Frame { _frameLabel      :: Label
                   , _frameEscapes    :: [Bool]
                   , _frameFormals    :: [FAccess]
                   , _frameLocals     :: [FAccess]
                   , _frameLocalsLen  :: Int
                   , _framePointer    :: Temp
                   , _frameStatic     :: FAccess
                   } deriving (Eq, Show)

lVarOffset :: Int -> Int
lVarOffset locals = locals * (- _WORDSIZE)

newFrame :: Label -> [Bool] -> Frame
newFrame name escs = frame
  where f next (frame, as) = (\(frame', a) -> (frame, a:as)) $ allocMem frame
        initFrame = Frame { _frameLabel    = name
                         , _frameEscapes   = escs
                         , _frameFormals   = formals
                         , _frameLocals    = []
                         , _frameLocalsLen = 0
                         , _framePointer   = RTemp RBX
                         , _frameStatic    = InFrame 0
                         }
        (frame, formals) = foldr f (initFrame, []) escs -- only works for escaping args

allocMem :: Frame -> (Frame, FAccess)
allocMem frame@Frame{..} = (frame', access)
  where access = InFrame $ lVarOffset (_frameLocalsLen + 1)
        frame' = frame { _frameLocals    = _frameLocals ++ [access]
                       , _frameLocalsLen = _frameLocalsLen + 1
                       }

allocReg :: Frame -> Temp -> (Frame, FAccess)
allocReg frame@Frame{..} temp = (frame', access)
  where access = InReg temp
        frame' = frame { _frameLocals    = _frameLocals ++ [access]
                       , _frameLocalsLen = _frameLocalsLen + 1
                       }



