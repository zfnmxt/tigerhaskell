module RuntimePrint (rtPrint, rtPrintInt, rtPrintErr)
where

import Data.Char (chr)
import System.IO (Handle, stdout, stderr, hPutChar)

import VMMonad (Mnd, lift, mfetch)
import Result (Res (IntRes, UnitRes))

hPrint :: Handle -> [Res] -> Mnd Res
hPrint hdl [IntRes pointer] =
    rtPrint' pointer
    where rtPrint' pointer =
		 do value <- mfetch pointer
		    case value of
		        0    -> return UnitRes
			char -> do lift $ hPutChar hdl $ chr char
				   rtPrint' $ pointer + 4

rtPrint :: [Res] -> Mnd Res
rtPrint [IntRes pointer] = hPrint stdout [IntRes pointer]

rtPrint _ =
    error "print: invalid arguments"

rtPrintErr :: [Res] -> Mnd Res
rtPrintErr [IntRes pointer] = hPrint stderr [IntRes pointer]

rtPrintErr _ =
    error "print_err: invalid arguments"

rtPrintInt :: [Res] -> Mnd Res
rtPrintInt [IntRes int] =
    do lift $ putStr $ show int
       return UnitRes

rtPrintInt _ =
    error "print_int: invalid arguments"
