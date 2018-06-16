{-# LANGUAGE RecordWildCards #-}

module Frame where

import Temp (Temp, Label)

_WORDSIZE = 4

data Access = InFrame Int | InReg Temp
  deriving (Eq, Show)

data Frame = Frame { _frameLabel     :: Label
                   , _frameEscapes   :: [Bool]
                   , _frameFormals   :: [Access]
                   , _frameLocals    :: [Access]
                   , _frameLocalsLen :: Int
                   , _framePointer   :: Temp
                   } deriving (Eq, Show)

data Reg = RAX | RBX | RCX | RDX | RSP | RBP | RSI
         | RDI | R8  | R9  | R10 | R11 | R12 | R13
         | R14 | R15


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
                         }
        (frame, formals) = foldr f (initFrame, []) escs -- only works for escaping args

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




