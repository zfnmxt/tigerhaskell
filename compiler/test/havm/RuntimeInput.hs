module RuntimeInput (rtGetChar)
where

import System.IO (getChar, isEOF)
import Data.Char (ord)

import VMMonad (Mnd, lift, mstore, mreserve)
import Result (Res (IntRes))

rtGetChar :: [Res] -> Mnd Res
rtGetChar [] =
    do eof <- lift $ isEOF
       case eof of
            True -> do pointer <- mreserve 4
                       mstore pointer 0
                       return $ IntRes pointer
            False -> do char <- lift $ getChar
                        pointer <- mreserve 8
                        mstore pointer $ ord char
                        mstore (pointer + 4) 0
                        return $ IntRes pointer

rtGetChar _ =
    error "getchar: invalid arguments"
