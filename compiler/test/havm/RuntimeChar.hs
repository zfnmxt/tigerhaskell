module RuntimeChar (rtChr, rtOrd)
where

import VMMonad (Mnd, mfetch, mstore, mreserve)
import Result (Res (IntRes))

import RuntimeCommon (rtError)

rtChr :: [Res] -> Mnd Res
rtChr [IntRes char] =
    if (not (0 <= char && char <= 255))
       then
	     do rtError "chr: character out of range"
		return $ IntRes char
       else
	     do pointer <- mreserve 8
       		mstore pointer char
       		mstore (pointer + 4) 0
       		return $ IntRes pointer

rtChr _ =
    error "chr: invalid arguments"

rtOrd :: [Res] -> Mnd Res
rtOrd [IntRes pointer] =
    do char <- mfetch pointer
       case char of
            0 -> return $ IntRes $ -1
	    n -> return $ IntRes n

rtOrd _ =
    error "ord: invalid arguments"
