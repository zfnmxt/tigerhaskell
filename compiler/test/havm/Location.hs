module Location (Loc (Loc, begin, end), 
		 incc, incl, incchar, incs, 
		 merge, advance)
where

import Position (Pos (Pos))
import qualified Position (incc, incl)

data Loc =
    Loc { begin :: Pos,
	  end   :: Pos }

incc :: Int -> Loc -> Loc
incc n loc =
    loc { end = Position.incc n $ end loc }

incl :: Int -> Loc -> Loc
incl n loc =
    loc { end = Position.incl n $ end loc }

incchar :: Char -> Loc -> Loc
incchar c loc
  | c == '\n' = incl 1 loc
  | otherwise = incc 1 loc

incs :: String -> Loc -> Loc
incs []       loc = loc
incs (c : cs) loc = incchar c (incs cs loc)

merge :: Loc -> Loc -> Loc
merge x y =
    x { end = end y }

advance :: Loc -> Loc
advance x =
    x { begin = end x }

instance Show Loc where
    show loc = (show $ begin loc) ++ "-" ++ (show $ end loc)
