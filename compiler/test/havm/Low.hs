module Low (lowExp, lowStms)
where

import Ir
import VMMonad (Mnd, lift)
import Annotation (Ann)
import Trace (awarn)

lowExp :: Int -> Exp Ann -> Mnd Bool
lowExp level (Binop _ _ e f) =
    do a <- lowExp (level + 1) e
       b <- lowExp (level + 1) f
       return $ a && b

lowExp level (Mem _ e) =
    lowExp (level + 1) e

lowExp level (Call l f es) =
    do awarn l ["invalid call"]
       sequence $ map (lowExp (level + 1)) es
       return False

lowExp level (ESeq l s e) =
    do awarn l ["invalid eseq"]
       lowStms (level + 1) [s]
       lowExp (level + 1) e
       return False

lowExp _ _ =
    return True

lowStms :: Int -> [Stm Ann] -> Mnd Bool
lowStms _ [] =
    return True

lowStms level ((Move _ (Temp _ _) (Call _ _ es)) : stms) =
    do a <- sequence $ map (lowExp (level + 1)) es
       b <- lowStms level stms
       return $ (and a) && b

lowStms level (Move _ e f : stms) =
    do a <- lowExp (level + 1) e
       b <- lowExp (level + 1) f
       c <- lowStms level stms
       return $ a && b && c

lowStms level (Sxp _ (Call _ _ es) : stms) =
    do a <- sequence $ map (lowExp (level + 1)) es
       b <- lowStms level stms
       return $ (and a) && b

lowStms level (Sxp _ e : stms) =
    do a <- lowExp (level + 1) e
       b <- lowStms level stms
       return $ a && b

lowStms level ((CJump _ _ e f _ (Name _ s)) : (Label _ s') : stms)
  | s == s' =
    do a <- lowExp (level + 1) e
       b <- lowExp (level + 1) f
       c <- lowStms level stms
       return $ a && b && c

lowStms level (CJump l _ e f _ _ : stms) =
    do awarn l ["invalid cjump"]
       lowExp (level + 1) e
       lowExp (level + 1) f
       lowStms level stms
       return False

lowStms level (Seq l stms : stms')
  | level == 0 =
    do a <- lowStms (level + 1) stms
       b <- lowStms level stms'
       return $ a && b
  | level /= 0 =
    do awarn l ["invalid seq"]
       lowStms (level + 1) stms
       lowStms level stms'
       return False

lowStms level (Label _ _ : stms) =
    lowStms level stms

lowStms level (Literal _ _ _ : stms) =
    lowStms level stms

lowStms level (stm : stms) =
    lowStms level stms
