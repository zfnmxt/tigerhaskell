module Preload (preloadStms)
where

import Ir
import VMMonad (Mnd, lift, mstoren, mreserve, cload, cstore)
import Annotation (Ann)

preloadExp :: Int -> Exp Ann -> Mnd ()
preloadExp level (Binop _ _ e f) =
    do preloadExp (level + 1) e
       preloadExp (level + 1) f

preloadExp level (Mem _ e) =
    preloadExp (level + 1) e

preloadExp level (Call _ f es) =
    sequence_ $ map (preloadExp (level + 1)) es

preloadExp level (ESeq _ s e) =
    do preloadStms (level + 1) [s]
       preloadExp (level + 1) e

preloadExp _ _ =
    return ()

preloadStms :: Int -> [Stm Ann] -> Mnd ()
preloadStms _ [] =
    return ()

preloadStms level (Move _ e f : stms) =
    do preloadExp (level + 1) e
       preloadExp (level + 1) f
       preloadStms level stms

preloadStms level (Sxp _ e : stms) =
    do preloadExp (level + 1) e
       preloadStms level stms

preloadStms level (CJump _ _ e f _ _ : stms) =
    do preloadExp (level + 1) e
       preloadExp (level + 1) f
       preloadStms level stms

preloadStms level (Seq _ stms : stms') =
    do preloadStms (level + 1) stms
       preloadStms level stms'

preloadStms 0 (Label _ l : stms) =
    do cload l (takeWhile preloadStms' stms)
       preloadStms 0 stms
    where preloadStms' (LabelEnd _) = False
	  preloadStms' _            = True

preloadStms level (Label _ l : stms) =
    do cload l stms
       preloadStms level stms

preloadStms level (Literal _ label string : statements) =
    do pointer <- mreserve $ ((length string) + 1) * 4
       cstore label pointer
       mstoren pointer $ string ++ [0]
       preloadStms level statements

preloadStms level (stm : stms) =
    preloadStms level stms
