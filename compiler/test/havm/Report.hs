module Report (reportAll, reportTime)
where

import Data.Map (Map, toList, findWithDefault)

reportAll :: Map String Int -> String
reportAll profile =
    foldl (++) ""  ["fetches from temporary : ", showInstr "temp"      , "\n",
		    "fetches from memory    : ", showInstr "mem"       , "\n",
		    "binary operations      : ", showInstr "binop"     , "\n",
		    "function calls         : ", showInstr "call"      , "\n",
		    "stores to temporary    : ", showInstr "move(temp)", "\n",
		    "stores to memory       : ", showInstr "move(mem)" , "\n",
		    "jumps                  : ", showInstr "jump"      , "\n",
		    "conditional jumps      : ", showInstr "cjump"     , "\n"]
    where showInstr instr = show $ findWithDefault 0 instr profile

reportTime :: Map String Int -> String
reportTime profile =
    "number of cycles : " ++ (show time)
    where result = snd $ unzip $ toList profile
	  final  = zipWith (*) result reportWeights
	  time   = sum final

reportWeights :: [Int]
reportWeights =
    [1, -- fetches from temporary
     10, -- fetches from memory
     1, -- binary operations
     1, -- function calls
     1, -- stores to temporary
     10, -- stores to memory
     1, -- jumps
     1] -- conditional jumps
