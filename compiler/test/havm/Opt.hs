module Opt (opt, OptVal (BVal, IVal))
where

import Config (package_string)
import System.Console.GetOpt
import Data.Map (Map, empty, insert)
import Data.Maybe (fromMaybe)

data Flag = Help
	  | TraceFd String
	  | DisplayFd String
	  | ProfileFd String
          | Version
	  | Low

data OptVal = BVal Bool
            | IVal Int

instance Eq Flag where
    (==) Help Help                   = True
    (==) (TraceFd a) (TraceFd b)     = (a == b)
    (==) (DisplayFd a) (DisplayFd b) = (a == b)
    (==) (ProfileFd a) (ProfileFd b) = (a == b)
    (==) Version Version             = True
    (==) Low Low                     = True
    (==) _ _                         = False

header  = "Usage: havm [OPTIONS] INPUT-FILE"

version_message = unlines
 [package_string,
  "Written by Robert Anisko.",
  "",
  "Copyright (C) 2002-2003  Robert Anisko",
  "Copyright (C) 2003-2007, 2009, 2011-2014"
  ++ "  EPITA Research and Development Laboratory (LRDE).",
  "This is free software; see the source for copying conditions.  There is NO",
  "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
  ]

options :: [OptDescr Flag]
options =
    [Option ['h'] ["help"]    (NoArg Help)     "display this help and exit",
     Option ['v'] ["version"] (NoArg Version)  "display the version and exit",
     Option ['d'] ["display"] (OptArg displayfd "FD") 
     	    	  	      "unparse on file descriptor FD",
     Option ['p'] ["profile"] (OptArg profilefd "FD") 
                              "print profiling information on FD",
     Option ['t'] ["trace"]   (OptArg tracefd "FD")    "trace execution on FD",
     Option ['l'] ["low"]     (NoArg Low) 
     	    	       	      "check low level intermediate representation"]

profilefd, displayfd, tracefd :: Maybe String -> Flag
profilefd = ProfileFd . fromMaybe "2"
displayfd = DisplayFd . fromMaybe "2"
tracefd   = TraceFd   . fromMaybe "2"

opt :: [String] -> IO (Map String OptVal, Maybe String)
opt arguments =
    case (getOpt Permute options arguments) of
        (o, n, [])   -> process (o, n)
	(_, _, errs) -> do putStr $ (concat errs) ++ usageInfo header options
			   return (empty, Nothing)

process :: ([Flag], [String]) -> IO (Map String OptVal, Maybe String)
process (opts, args)
    | Help `elem` opts =
	do putStr $ usageInfo header options
	   return (empty, Nothing)
    | Version `elem` opts =
	do putStr version_message
	   return (empty, Nothing)
    | (length args) == 1 =
	return (extract opts empty, Just $ head args)

process _ =
    do putStrLn header
       return (empty, Nothing)

extract :: [Flag] -> Map String OptVal -> Map String OptVal
extract [] fm =
    fm

extract ((TraceFd fd) : flags) fm =
    extract (flags) (insert "trace" (IVal (read fd)) fm)

extract ((DisplayFd fd) : flags) fm =
    extract (flags) (insert "display" (IVal (read fd)) fm)

extract ((ProfileFd fd) : flags) fm =
    extract (flags) (insert "profile" (IVal (read fd)) fm)

extract (Low : flags) fm =
    extract (flags) (insert "low" (BVal True) fm)
