{-# LANGUAGE RecordWildCards #-}

module Frame where

import Temp
import Registers
import Tree

_WORDSIZE = 4
_R_RET    = RTemp RAX
_R_FP     = RTemp RBX

data FAccess = InFrame Int | InReg Temp
  deriving (Eq, Show)

data Frame = Frame { _frameLabel      :: Label
                   , _frameFormals    :: [FAccess]
                   , _frameStatic     :: FAccess
                   , _frameWordSize   :: Int
                   , _frameLocalNum   :: Int
                   } deriving (Show, Eq)


lVarOffset :: Int -> Int
lVarOffset locals = locals * (- _WORDSIZE)

newFrame :: Label -> [Bool] -> Frame
newFrame name escs = frame
  where f next (frame, as) = (\(frame', a) -> (frame, a:as)) $ allocMem frame
        initFrame = Frame { _frameLabel    = name
                         , _frameFormals   = formals
                         , _frameStatic    = InFrame 0
                         , _frameWordSize  = _WORDSIZE
                         , _frameLocalNum  = 0
                         }
        (frame, formals) = foldr f (initFrame, []) escs -- only works for escaping args

allocMem :: Frame -> (Frame, FAccess)
allocMem frame@Frame{..} = (frame', access)
  where access = InFrame $ lVarOffset (_frameLocalNum + 1)
        frame' = frame { _frameLocalNum = _frameLocalNum + 1}

allocReg :: Frame -> Temp -> (Frame, FAccess)
allocReg frame@Frame{..} temp = (frame, InReg temp)
  where access = InReg temp

procEntryExit1 :: Frame -> TreeStm -> TreeStm
procEntryExit1 _ _ = StmExp (Const 0)

externCall :: String -> [TreeExp] -> TreeExp
externCall f args = Call (Name (NamedLabel f)) args
