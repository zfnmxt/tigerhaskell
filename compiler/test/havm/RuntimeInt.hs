module RuntimeInt (rtNot)
where

import VMMonad (Mnd)
import Result (Res (IntRes))

rtNot :: [Res] -> Mnd Res
rtNot [IntRes 0] =
    return $ IntRes 1

rtNot [IntRes _] =
    return $ IntRes 0

rtNot _ =
    error "not: invalid arguments"
