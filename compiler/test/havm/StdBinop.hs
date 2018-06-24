module StdBinop (binop)
where

import Data.Int
import Data.Word
import Data.Bits

import Ir

modulo32 op a b =
    fromIntegral((fromIntegral a :: Int32) `op` (fromIntegral b :: Int32))

unsigned op a b = fromIntegral((fromIntegral a :: Word) `op` b)

binop :: Op -> Int -> Int -> Int
binop Add     = modulo32 (+)
binop Sub     = modulo32 (-)
binop Mul     = modulo32 (*)
binop Div     = modulo32 quot
binop And     = modulo32 (.&.)
binop Or      = modulo32 (.|.)
-- Logical shifts.  When shifting Words (unsigned integers) instead of
-- Ints (signed integers), the shifts become logical shifts (see
-- http://rosettacode.org/wiki/Bitwise_operations#Haskell).
binop Lshift  = unsigned shiftL
binop Rshift  = unsigned shiftR
-- Arithmetic right shift.
binop Arshift = shiftR
binop Xor     = xor
