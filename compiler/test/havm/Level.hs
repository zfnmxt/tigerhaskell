module Level (Level (Level, level),
	      initialize,
	      lstore, lfetch)
where

import Ir
import Data.Map (Map, empty, insert, findWithDefault)

data Level = Level { level :: Map String Int }

initialize :: Level
initialize = Level { level = empty }

lstore :: Level -> String -> Int -> Level
lstore l label depth =
    l { level = insert label depth (level l) }

lfetch :: Level -> String -> Int
lfetch l label =
    findWithDefault 0 label (level l) 
