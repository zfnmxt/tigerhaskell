module Position (Pos (Pos, line, column), incc, incl)
where

data Pos =
    Pos { line :: Int,
	  column :: Int }

incc :: Int -> Pos -> Pos
incc n pos =
    pos { column = column pos + n }

incl :: Int -> Pos -> Pos
incl n pos =
    pos { line = line pos + n,
	  column = 0 }

instance Show Pos where
    show pos = (show $ line pos) ++ "." ++ (show $ column pos)
