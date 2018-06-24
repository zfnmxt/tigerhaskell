module Annotation (Ann (NoAnn, LevelAnn, LocationAnn),
		   annExp, annStm, levelAnn, locationAnn)
where

import Ir
import Location (Loc)

data Ann = NoAnn
	 | LevelAnn Int
	 | LocationAnn Loc

annExp :: Exp Ann -> Ann
annExp (Const a _) = a
annExp (Name a _) = a
annExp (Temp a _) = a
annExp (Binop a _ _ _) = a
annExp (Mem a _) = a
annExp (Call a _ _) = a
annExp (ESeq a _ _) = a

annStm :: Stm Ann -> Ann
annStm (Move a _ _) = a
annStm (Sxp a _) = a
annStm (Jump a _) = a
annStm (CJump a _ _ _ _ _) = a
annStm (Seq a _) = a
annStm (Label a _) = a
annStm (Literal a _ _) = a

levelAnn :: Ann -> Int
levelAnn (LevelAnn n) = n

locationAnn :: Ann -> Loc
locationAnn (LocationAnn l) = l

instance Show Ann where
    show NoAnn           = ""
    show (LevelAnn n)    = unwords ["/*", "level:", show n, "*/: "]
    show (LocationAnn l) = show l ++ ": "
