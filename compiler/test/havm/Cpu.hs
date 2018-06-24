module Cpu (Registry, initialize, rfetch, rstore, rpush, rpop)
where

import Data.Map (Map, empty, insert, lookup)

data Registry = Registry { fp :: Int,
			   rv :: Int,
			   sp :: Int,
			   temp :: [Map String Int] }

initialize :: Int -> Int -> Registry
initialize heapsize' stacksize' = Registry { fp = heapsize' + stacksize' - 4,
					     rv = 0,
					     sp = heapsize' + stacksize' - 4,
					     temp = [empty] }

rfetch :: Registry -> String -> Int
rfetch r "fp"  = fp r
rfetch r "$fp" = fp r
rfetch r "rv"  = rv r
rfetch r "$v0" = rv r
rfetch r "sp"  = sp r
rfetch r "$sp" = sp r
rfetch r s =
    case Data.Map.lookup s (head $ temp r) of
        (Just i) -> i
	_        -> error ("no such temp: " ++ show s)

rstore :: Registry -> String -> Int -> Registry
rstore r "fp"  i = r { fp = i }
rstore r "$fp" i = r { fp = i }
rstore r "rv"  i = r { rv = i }
rstore r "$v0" i = r { rv = i }
rstore r "sp"  i = r { sp = i }
rstore r "$sp" i = r { sp = i }
rstore r s i    = r { temp = (insert s i (head $ temp r)) : (tail $ temp r) }

rpush :: Registry -> Registry
rpush r = r { temp = (head $ temp r) : temp r }

rpop :: Registry -> Registry
rpop r = r { temp = tail $ temp r }
