module High (highExp, highStms)
where

import System.IO (stderr, hPutStrLn)

import Ir
import Print
import VMMonad (Mnd, lift)
import Trace (awarn)
import Annotation (Ann, annExp)

highExp :: Int -> Exp Ann -> Mnd Bool
highExp level (Binop _ _ e f) =
    do a <- highExp (level + 1) e
       b <- highExp (level + 1) f
       return $ a && b

highExp level (Mem _ e) =
    highExp (level + 1) e

highExp level (Name _ e) =
    return True;

highExp level (Call l f es) =
    do a  <- highExp (level + 1) f
       a' <- case f of
	       Name _ _  -> return True
	       _         -> do awarn (annExp f) ["invalid call destination:",
						 show f]
			       return False
       b  <- sequence $ map (highExp (level + 1)) es
       return $ a && a' && (and b)

highExp level (ESeq l s e) =
    do a <- highExp (level + 1) e
       b <- highStms (level + 1) [s]
       return $ a && b

highExp _ _ =
    return True

highStms :: Int -> [Stm Ann] -> Mnd Bool
highStms level ss =
    do a <- sequence $ map (highStm level) ss
       return (and a)

highStm :: Int -> Stm Ann -> Mnd Bool
highStm level (Move l dest src) =
    do a  <- highExp (level + 1) dest
       a' <- case dest of
	       Temp _ _ -> return True
	       Mem _ _  -> return True
	       _        -> do awarn (annExp dest) ["invalid move destination:",
						   show dest]
			      return False
       b <- highExp (level + 1) src
       return $ a && a' && b

highStm level (Sxp _ e) =
    do a <- highExp (level + 1) e
       return a

-- CJump must branch to Names.
highStm level (CJump l _ e f l1 l2) =
    do a  <- highExp (level + 1) e
       b  <- highExp (level + 1) f
       c  <- highExp level l1
       d  <- highExp level l2
       c' <- case l1 of
	      Name _ _ -> return True
              _        -> do awarn (annExp l1) ["invalid cjump destination:",
						show l1]
			     return False
       d' <- case l2 of
	      Name _ _ -> return True
	      _        -> do awarn (annExp l2) ["invalid cjump destination:",
						show l2]
			     return False
       return $ a && b && c && d && c' && d'

highStm level (Jump l d) =
    do a  <- highExp (level + 1) d
       a' <- case d of
	       Name _ _ -> return True
	       _        -> do awarn (annExp d) ["invalid jump destination:",
						show d]
			      return False
       return $ a && a'

highStm level (Seq l stms) =
    do a <- highStms (level + 1) stms
       return a

highStm level (Label _ _) =
    do return True

highStm level (LabelEnd _) =
    do return True

highStm level (Literal _ _ _) =
    do return True
