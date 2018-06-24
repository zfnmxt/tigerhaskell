module Havm (main, havmString)
where

import System.IO (Handle, IOMode (ReadMode), BufferMode (LineBuffering),
		  stderr, stdout, openFile,
		  hGetContents, hClose, hPutStr, hPutStrLn, hSetBuffering,
		  hSetBinaryMode)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Posix (exitImmediately)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (Fd))
import Foreign.C.Types (CInt)
import Data.Map (Map, findWithDefault, member, (!))

import Ir
import High (highStms)
import Low (lowStms)
import Opt (opt, OptVal (IVal))
import Eval (evalExp, evalStm, evalStms)
import Scan (end, scan)
import VMMonad (Mnd, run, lift, rstore, cfind, optset, opttell, profresult,
                setProfileHandle, getProfileHandle,
		setDisplayHandle, getDisplayHandle,
		setTraceHandle, getTraceHandle)
import Parse (parse)
import Plain (plainStms)
import Print
import Trace (trace, warn)
import Token
import Report (reportAll, reportTime)
import Preload (preloadStms)
import Location (Loc (Loc))
import Position (Pos (Pos))
import Annotation (Ann (NoAnn))
import Opt (OptVal (BVal))

scanM :: String -> IO [Tok]
scanM text =
    return $ end $ scan text loc
    where loc = Loc (Pos 1 0) (Pos 1 0)

parseM :: [Tok] -> IO [Stm Ann]
parseM tokens =
    return $ parse tokens

unparseM :: [Stm Ann] -> Mnd ()
unparseM stms =
    do d <- getDisplayHandle
       case d of
	    Just h -> do lift $ hPutStrLn h "/* Unparsing.  */"
			 lift $ hPutStr h $ show stms
			 lift $ hPutStrLn h ""
	    Nothing -> return ()

checkHighM :: [Stm Ann] -> Mnd ()
checkHighM stms =
    do high <- highStms 0 stms
       case high of
            True  -> return ()
	    False -> lift $ exitImmediately $ ExitFailure 129

checkLowM :: [Stm Ann] -> Mnd ()
checkLowM stms =
    do l <- opttell "low" (BVal False)
       case l of
	 BVal True  -> do low <- lowStms 0 stms
			  case low of
                            True  -> return ()
                            False -> lift $ exitImmediately $ ExitFailure 129
	 BVal False -> return ()
         _ -> error "checkLowM: bad value for `low' option"

profileM :: Mnd ()
profileM =
    do p <- getProfileHandle
       case p of
	 Just h ->
             do prof <- profresult
                lift $ hPutStrLn h "/* Profiling.  */"
		lift $ hPutStrLn h $ reportAll prof
		lift $ hPutStrLn h "/* Execution time.  */"
		lift $ hPutStrLn h $ reportTime prof
	 Nothing -> return ()

-- Extract the FD corresponding to the option s in the fm, defaulting to -1.
fdFrom :: Map String OptVal -> String -> Int
fdFrom fm s =
    case findWithDefault (IVal (-1)) s fm of
      IVal x -> x
      _      -> error $ "fdFrom: bad value for `" ++ s ++ "' option"

-- Store the file descriptor for s in fm using storeHandle
setHandle :: (Handle -> Mnd ()) -> Map String OptVal -> String -> Mnd ()
setHandle storeHandle fm s =
     setHandle_ (fdFrom fm s)
     where 
         -- If the fd is valid, apply storeHandle to it.
         setHandle_ fd = 
           if fd >= 0
             then do ph <- lift (fdToHandle $ Fd (fromIntegral fd))
                     storeHandle ph
             else return ()

-- Close a (maybe) file handle.
closeHandle :: Maybe Handle -> Mnd ()
closeHandle mh =
  case mh of
    Just h -> lift $ hClose h
    Nothing -> return ()

-- Evaluate the program.
evalM :: Map String OptVal -> [Stm Ann] -> Mnd ()
evalM fm stms =
    do optset "low" $ findWithDefault (BVal False) "low" fm
       setHandle setProfileHandle fm "profile"
       setHandle setDisplayHandle fm "display"
       setHandle setTraceHandle   fm "trace"
       trace ["checkingLow"]
       checkLowM stms
       trace ["plaining"]
       stms <- plainStms stms
       trace ["unparsing"]
       unparseM stms
       trace ["checking"]
       checkHighM stms
       preloadStms 0 stms
       rstore "i0" 0
       trace ["evaling"]
       evalExp $ Call NoAnn (Name NoAnn "main") []
       profileM
       getProfileHandle >>= closeHandle
       getDisplayHandle >>= closeHandle
       getTraceHandle   >>= closeHandle

main :: IO ()
main =
    do args <- getArgs
       (opts, input) <- opt args
       case input of
            (Just input) -> do hSetBuffering stderr LineBuffering
			       hSetBuffering stdout LineBuffering
			       hSetBinaryMode stdout True
			       handle <- openFile input ReadMode
			       text <- hGetContents handle
			       toks <- scanM text
			       ir <- parseM toks
			       run $ evalM opts ir
			       hClose handle
			       return ()
	    (Nothing) -> return ()

havmString :: String -> IO ()
havmString text = do
       (opts, input) <- opt ["foo"]
       case input of
            (Just input) -> do hSetBuffering stderr LineBuffering
			       hSetBuffering stdout LineBuffering
			       hSetBinaryMode stdout True
			       toks <- scanM text
			       ir <- parseM toks
			       run $ evalM opts ir
			       return ()
	    (Nothing) -> return ()
