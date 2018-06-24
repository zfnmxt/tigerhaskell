module Runtime (rtLib)
where

import Data.Map (Map, fromList)

import VMMonad (Mnd)
import Result
import RuntimeInt (rtNot)
import RuntimeChar (rtChr, rtOrd)
import RuntimeInput (rtGetChar)
import RuntimePrint (rtPrint, rtPrintErr, rtPrintInt)
import RuntimeString (rtSize, rtConcat, rtCompare, rtStringEqual, rtSubstring)
import RuntimeSystem (rtExit, rtFlush, rtMalloc, rtInitArray)

rtLib :: Map String ([Res] -> Mnd Res)
rtLib = fromList [("chr"        , rtChr),
		  ("concat"     , rtConcat),
		  ("exit"       , rtExit),
		  ("flush"      , rtFlush),
		  ("getchar"    , rtGetChar),
		  ("init_array" , rtInitArray),
		  ("malloc"     , rtMalloc),
		  ("not"        , rtNot),
		  ("_not"       , rtNot),
		  ("ord"        , rtOrd),
		  ("print"      , rtPrint),
                  ("print_err"  , rtPrintErr),
		  ("print_int"  , rtPrintInt),
		  ("printint"   , rtPrintInt),
		  ("size"       , rtSize),
		  ("strcmp"     , rtCompare),
                  ("streq"      , rtStringEqual),
		  ("stringEqual", rtStringEqual),
		  ("substring"  , rtSubstring)]
