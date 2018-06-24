module Ir (Exp (Const, Name, Temp, Binop, Mem, Call, ESeq),
	   Stm (Move, Sxp, Jump, CJump, Seq, Label, LabelEnd, Literal),
	   Op (Add, Sub, Mul, Div, And, Or, Lshift, Rshift, Arshift, Xor),
	   Relop (Eq, Ne, Lt, Gt, Le, Ge, Ult, Ule, Ugt, Uge))
where

data Exp a = Const a Int
       	   | Name a String
 	   | Temp a String
           | Binop { ba :: a,
		     op :: Op,
		     bleft :: Exp a,
		     bright :: Exp a }
	   | Mem a (Exp a)
           | Call { ca :: a,
		    fun :: Exp a,
		    arg :: [Exp a] }
	   | ESeq { ea :: a,
		    stm :: Stm a,
		    exp :: Exp a }

data Stm a = Move { ma :: a,
		    lval :: Exp a,
		    rval :: Exp a }
	   | Sxp a (Exp a)
	   | Jump a (Exp a)
	   | CJump { cja :: a,
		     rop :: Relop,
		     cleft :: Exp a,
		     cright :: Exp a,
		     iftrue :: Exp a,
		     iffalse :: Exp a }
	   | Seq a [Stm a]
	   | Label { la :: a,
		     name :: String }
	   | LabelEnd a
	   | Literal { lita :: a,
		       litname :: String,
		       litcontent :: [Int] }

data Op = Add
	| Sub
	| Mul
	| Div
	| And
	| Or
	| Lshift
	| Rshift
	| Arshift
	| Xor

data Relop = Eq
	   | Ne
	   | Lt
	   | Gt
	   | Le
	   | Ge
	   | Ult
	   | Ule
	   | Ugt
	   | Uge

instance Show Op where
    show Add     = "add"
    show Sub     = "sub"
    show Mul     = "mul"
    show Div     = "div"
    show And     = "and"
    show Or      = "or"
    show Lshift  = "lshift"
    show Rshift  = "rshift"
    show Arshift = "arshift"
    show Xor     = "xor"

instance Show Relop where
    show Eq  = "eq"
    show Ne  = "ne"
    show Lt  = "lt"
    show Gt  = "gt"
    show Le  = "le"
    show Ge  = "ge"
    show Ult = "ult"
    show Ule = "ule"
    show Ugt = "ugt"
    show Uge = "uge"
