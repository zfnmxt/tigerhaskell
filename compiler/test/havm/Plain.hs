module Plain (plainStms)
where

import Ir
import VMMonad (Mnd)
import Annotation (Ann (LocationAnn), annStm, locationAnn)
import Location (merge)

-- FIXME: It is not always needed to create a Seq here.
plainExp :: Exp Ann -> Mnd (Exp Ann)
plainExp (ESeq ann stm exp) =
    do stms <- plainStms [stm]
       exp <- plainExp exp
       return $ ESeq ann (Seq
			  -- Location is the range of the created Seq.
			  (LocationAnn $ merge (locationAnn $ annStm $ head $ stms)
			   (locationAnn $ annStm $ last $ stms))
			  stms) exp

plainExp exp =
    return exp

plainStms :: [Stm Ann] -> Mnd [Stm Ann]
plainStms [] =
    return []

plainStms (Move ann e f : stms) =
    do e <- plainExp e
       f <- plainExp f
       stms <- plainStms stms
       return $ Move ann e f : stms

plainStms (Sxp ann exp : stms) =
    do exp <- plainExp exp
       stms <- plainStms stms
       return $ Sxp ann exp : stms

plainStms (CJump ann a b c d e : stms) =
    do b <- plainExp b
       c <- plainExp c
       d <- plainExp d
       e <- plainExp e
       stms <- plainStms stms
       return $ CJump ann a b c d e : stms

plainStms (Jump ann exp : stms) =
    do exp <- plainExp exp
       stms <- plainStms stms
       return $ Jump ann exp : stms

plainStms (Seq _ stms : stms') =
    do stms <- plainStms stms
       stms' <- plainStms stms'
       return $ stms ++ stms'

plainStms (stm : stms) =
    do stms <- plainStms stms
       return $ stm : stms
