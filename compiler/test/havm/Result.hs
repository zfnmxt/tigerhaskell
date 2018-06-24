module Result (Res (IntRes, UnitRes, SymbolicRes))
where

data Res = IntRes Int
	 | UnitRes
	 | SymbolicRes String

instance Show Res where
    show (IntRes i)      = show i
    show UnitRes         = "()"
    show (SymbolicRes s) = show s
