module Token (Tok (TokBinop, TokCJump, TokCall, TokCallEnd, TokConst, TokESeq,
		   TokEnd, TokExp, TokFrame, TokInt, TokJump, TokLabel,
		   TokLabelEnd, TokLiteral, TokMem, TokMove, TokName, TokOp,
		   TokRelop, TokSeq, TokSeqEnd, TokSize, TokString, TokTemp))
where

import Ir (Op, Relop)
import Location (Loc (Loc))

data Tok = TokBinop Loc
         | TokCJump Loc
	 | TokCall Loc
	 | TokCallEnd Loc
	 | TokConst Loc
	 | TokESeq Loc
	 | TokEnd Loc
	 | TokExp Loc
	 | TokFrame Loc
	 | TokInt (Int, Loc)
	 | TokJump Loc
	 | TokLabel Loc
	 | TokLabelEnd Loc
	 | TokLiteral (String, Loc)
	 | TokMem Loc
	 | TokMove Loc
	 | TokName Loc
	 | TokOp (Op, Loc)
	 | TokRelop (Relop, Loc)
	 | TokSeq Loc
	 | TokSeqEnd Loc
	 | TokSize Loc
	 | TokString (String, Loc)
	 | TokTemp Loc

instance Show Tok where
    show (TokBinop l)        = (show l) ++ ":" ++ "binop"
    show (TokCJump l)        = (show l) ++ ":" ++ "cjump"
    show (TokCall l)         = (show l) ++ ":" ++ "call"
    show (TokCallEnd l)      = (show l) ++ ":" ++ "call end"
    show (TokConst l)        = (show l) ++ ":" ++ "const"
    show (TokESeq l)         = (show l) ++ ":" ++ "eseq"
    show (TokEnd l)          = (show l) ++ ":" ++ "end"
    show (TokExp l)          = (show l) ++ ":" ++ "exp"
    show (TokFrame l)        = (show l) ++ ":" ++ "frame"
    show (TokInt (i, l))     = (show l) ++ ":" ++ (show i)
    show (TokJump l)         = (show l) ++ ":" ++ "jump"
    show (TokLabel l)        = (show l) ++ ":" ++ "label"
    show (TokLabelEnd l)     = (show l) ++ ":" ++ "label end"
    show (TokLiteral (s, l)) = (show l) ++ ":" ++ (show s)
    show (TokMem l)          = (show l) ++ ":" ++ "mem"
    show (TokMove l)         = (show l) ++ ":" ++ "move"
    show (TokName l)         = (show l) ++ ":" ++ "name"
    show (TokOp (o, l))      = (show l) ++ ":" ++ (show o)
    show (TokRelop (o, l))   = (show l) ++ ":" ++ (show o)
    show (TokSeq l)          = (show l) ++ ":" ++ "seq"
    show (TokSeqEnd l)       = (show l) ++ ":" ++ "seq end"
    show (TokSize l)         = (show l) ++ ":" ++ "size"
    show (TokString (s, l))  = (show l) ++ ":" ++ s
    show (TokTemp l)         = (show l) ++ ":" ++ "temp"
