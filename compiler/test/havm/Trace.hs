module Trace (trace, atrace, warn, awarn)
where

import VMMonad (Mnd, lift, opttell, getTraceHandle)
import Annotation
import System.IO (Handle, hPutStrLn, stderr)

-- Report the args when traces are enabled.
warn :: [String] -> Mnd ()
warn = hwarn stderr

hwarn :: Handle -> [String] -> Mnd ()
hwarn h es =
    lift $ hPutStrLn h $ unwords $ es

-- Report the args when traces are enabled.
trace :: [String] -> Mnd ()
trace es = 
    do t <- getTraceHandle
       case t of
	    Just h  -> hwarn h es
	    Nothing -> return ()

-- Same but with annotations (probably locations).
awarn :: Ann -> [String] -> Mnd ()
awarn ann es =
    warn $ show ann : es

-- Report the args when traces are enabled.
atrace :: Ann -> [String] -> Mnd ()
atrace ann es = trace $ show ann : es
