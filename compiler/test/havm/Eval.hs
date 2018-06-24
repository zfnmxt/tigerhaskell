module Eval (evalExp, evalStm, evalStms)
where

import Data.Map (lookup)

import Ir
import Print
import VMMonad (Mnd,
	        lift,
	        rfetch, rstore, rpush, rpop,
	        mfetch, mstore,
	        cload, cfind, cstore, cfetch)
import Trace (atrace)
import Result (Res (IntRes, UnitRes))
import Profile (profileExp, profileStm)
import Runtime (rtLib)
import Annotation (Ann, annExp)

import StdBinop (binop)
import StdRelop (relop)

shipArgs :: Int -> [Res] -> Mnd ()
shipArgs _ [] =
    return ()

shipArgs n ((IntRes r) : rs) =
    do rstore ("i" ++ (show n)) r
       args <- shipArgs (n + 1) rs
       return ()

evalInt :: Exp Ann -> Mnd Int
evalInt exp =
  do res <- evalExp exp
     case res of
       (IntRes r) -> return r
       _          -> error (show (annExp exp)
			    ++ "expected an integer: "
			    ++ show exp)

evalExp :: Exp Ann -> Mnd Res
evalExp exp@(Const ann i) =
    do atrace ann ["const", show i]
       profileExp exp
       return $ IntRes i

evalExp exp@(Name ann label) =
    do ptr <- cfetch label
       atrace ann ["name", label, "=", show ptr]
       profileExp exp
       return $ IntRes ptr

evalExp exp@(Temp ann s) =
    do i <- rfetch s
       atrace ann["temp", s, "=", show i]
       profileExp exp
       return $ IntRes i

evalExp exp@(Binop ann op left right) =
    do i <- evalInt left
       j <- evalInt right
       atrace ann ["binop", show op, show i, show j]
       profileExp exp
       return $ IntRes $ binop op i j

evalExp exp@(Mem ann e) =
    do r <- evalInt e
       i <- mfetch r
       atrace ann ["mem", show r, "=", show i]
       profileExp exp
       return $ IntRes i

-- Invoking a function is of course a bit tricky: we must provide all
-- the services that are not yet implemented at IR level, such as
-- saving and restoring the registers.
--
-- There are several constraints to respect.
--
-- 1. Evaluate the arguments before saving the registers
--
-- This is because some of the arguments can have side effects
-- that must be preserved.  For instance
--
--  call print_int ( eseq (a := a + 1, a) )
--  call print_int ( eseq (a := a + 1, a) )
--
-- will print twice the same value (if a is in a temporary), since...
-- since the temp holding "a" will be saved.
-- Therefore, evaluate the arguments (for side effects) before saving
-- the registers.
--
-- 2. Preserve old i0, i1 etc.
--
-- If you have a call like
--
--   call add (call add (10, 20), call add (100, 200))
--
-- then once the outer "call add" evaluated the value of i1 for "call
-- add" is known: 30.  But then, if you call the second inner "add", it will
-- override the i1 for "add" with its own: 100.  Therefore, pay extra
-- attention not to kill the actual arguments.  The easiest is simply
-- to evaluate all the arguments, return their values as a simple list,
-- and only immediately before invoking the body of the function,
-- defining the "i1" etc.  Do not intermix the two sequences (e.g.,
-- evaluate and store i1, then evaluate and store i2 etc.): you would
-- find the previous problem.
evalExp exp@(Call ann (Name _ l) exps) =
    do -- Eval the args before saving the context.
       args <- sequence $ map evalExp exps
       atrace ann ["call", "(", "name", l, ")", show args]
       shipArgs 0 args
       rv <- case Data.Map.lookup l rtLib of
        (Just f) -> do rv <- f args
		       return rv
	_        -> do rpush              -- Save registers
		       srv <- rfetch "rv" -- Save return value
                       profileExp exp
		       stms <- cfind l    -- Find the function code
                       evalStms stms      -- Execute code
		       rv <- rfetch "rv"  -- Read return value register
		       rpop               -- Restore registers
		       rstore "rv" srv    -- Restore old return value register
		       return $ IntRes $ rv
       atrace ann ["end call", "(", "name", l, ")", show args,  "=", show rv]
       return rv        -- Return value

evalExp exp@(ESeq ann s e) =
    do atrace ann ["eseq"]
       profileExp exp
       evalStm s
       r <- evalExp e
       return r

evalStms :: [Stm Ann] -> Mnd ()
evalStms [] =
    return ()

evalStms (stm@(Jump _ _) : stms) =
    do evalStm stm

evalStms (stm@(CJump _ _ _ _ _ _ ) : stms) =
    do evalStm stm

evalStms (stm : stms) =
    do evalStm stm
       evalStms stms

evalStm :: Stm Ann -> Mnd ()
evalStm stm@(Move ann (Temp _ t) e) =
    do r <- evalInt e
       atrace ann ["move", "(", "temp", t, ")", show r]
       profileStm stm
       rstore t r

evalStm stm@(Move ann (Mem _ e) f) =
    do r <- evalInt e
       s <- evalInt f
       atrace ann ["move", "(", "mem", show r, ")", show s]
       profileStm stm
       mstore r s

evalStm stm@(Sxp ann e) =
    do r <- evalExp e
       atrace ann ["sxp", show r]
       profileStm stm
       return ()

evalStm stm@(Jump ann (Name _ l)) =
    do atrace ann ["jump", "(", "name", l, ")"]
       profileStm stm
       stms <- cfind l
       evalStms stms

evalStm stm@(CJump ann rop left right (Name _ true) (Name _ false)) =
    do i <- evalInt left
       j <- evalInt right
       atrace ann ["cjump", show rop, show i, show j,
		     "(", "name", true, ")",
		     "(", "name", false, ")"]
       profileStm stm
       case relop rop i j of
          True -> do stms <- cfind true
		     evalStms stms
	  False -> do stms <- cfind false
		      evalStms stms

evalStm stm@(Seq ann stms) =
    do atrace ann ["seq"]
       profileStm stm
       r <- evalStms stms
       atrace ann ["seq end"]
       return r

evalStm (Label ann name) =
    do atrace ann ["label", name]
       return ()

evalStm (Literal ann name _) =
    do atrace ann ["literal", name]
       return ()
