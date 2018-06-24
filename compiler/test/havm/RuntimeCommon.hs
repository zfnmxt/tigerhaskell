module RuntimeCommon (rtError)
where

import VMMonad (Mnd, lift, mstore, mreserve)
import Result (Res (IntRes, UnitRes))

import System.Posix (exitImmediately)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO (hPutStrLn, stderr)

-- Runtime error (used when a runtime assertion fails)
rtError :: String -> Mnd ()
rtError msg =
    do lift $ hPutStrLn stderr msg
       lift $ exitImmediately $ ExitFailure 120
