module RuntimeSystem (rtExit, rtFlush, rtMalloc, rtInitArray)
where

import System.IO (hFlush, stderr, stdout)
import System.Posix (exitImmediately)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))

import VMMonad (Mnd, lift, mstore, mreserve)
import Result (Res (IntRes, UnitRes))

rtExit :: [Res] -> Mnd Res
rtExit [IntRes status] =
    do lift $ hFlush stderr
       lift $ hFlush stdout
       lift $ exitImmediately $ ExitFailure status
       return UnitRes

rtExit _ =
    error "exit: invalid arguments"

rtFlush :: [Res] -> Mnd Res
rtFlush [] =
    do lift $ hFlush stdout
       return UnitRes

rtFlush _ =
    error "flush: invalid arguments"

rtMalloc :: [Res] -> Mnd Res
rtMalloc [IntRes size] =
    do pointer <- mreserve size
       return $ IntRes pointer

rtMalloc _ =
    error "malloc: invalid arguments"

rtInitArray :: [Res] -> Mnd Res
rtInitArray [IntRes size, IntRes value] =
    do pointer <- mreserve $ size * 4
       rtInitArray' pointer value size
       return $ IntRes pointer
    where rtInitArray' pointer value 0 =
	      return ()

          rtInitArray' pointer value size =
	      do mstore pointer value
		 rtInitArray' (pointer + 4) value (size - 1)

rtInitArray _ =
    error "init_array: invalid arguments"
