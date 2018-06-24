module Code (Code,
	     initialize,
	     cload, cfind,
	     cstore, cfetch)
where

import Data.Map (Map, empty, insert, lookup)

import Ir
import Annotation (Ann)

data Code = Code { dat :: Map String Int,
		   code :: Map String ([Stm Ann]) }

initialize :: Code
initialize = Code { dat = empty,
		    code = empty }

cload :: Code -> String -> [Stm Ann] -> Code
cload c k stms =
    c { code = insert k stms (code c) }

cfind :: Code -> String -> [Stm Ann]
cfind c k =
    case Data.Map.lookup k (code c) of
        (Just (stms)) -> stms
	Nothing       -> error $ unwords ["Error: no label", k]

cstore :: Code -> String -> Int -> Code
cstore c k i =
    c { dat = insert k i (dat c) }

cfetch :: Code -> String -> Int
cfetch c k =
    case Data.Map.lookup k (dat c) of
        (Just i) -> i
	Nothing  -> error $ unwords ["Error: no adress associated with", k]
