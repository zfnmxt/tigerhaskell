module Profile (profileExp, profileStm)
where

import Ir
import VMMonad (Mnd, lift, profinc, opttell, getProfileHandle)
import Annotation (Ann)

profileExp :: Exp Ann -> Mnd ()
profileExp e =
    do t <- getProfileHandle
       case t of
         Nothing -> return ()
	 Just _  -> case e of
		      (Temp _ _)      -> profinc "temp"
		      (Binop _ _ _ _) -> profinc "binop"
		      (Mem _ _)       -> profinc "mem"
		      (Call _ _ _)    -> profinc "call"
		      _               -> return ()

profileStm :: Stm Ann -> Mnd ()
profileStm e =
    do t <- getProfileHandle
       case t of
	 Nothing -> return ()
	 Just _  -> case e of
		      (Move _ (Temp _ _) _) -> profinc "move(temp)"
                      (Move _ (Mem _ _) _)  -> profinc "move(mem)"
		      (Jump _ _)            -> profinc "jump"
		      (CJump _ _ _ _ _ _)   -> profinc "cjump"
		      _                     -> return ()
