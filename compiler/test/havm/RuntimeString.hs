module RuntimeString (rtSize, rtConcat, rtCompare, rtStringEqual, rtSubstring)
where

import VMMonad (Mnd, mfetch, mstore, mreserve)
import Result (Res (IntRes))

import RuntimeCommon (rtError)

rtSize :: [Res] -> Mnd Res
rtSize [IntRes pointer] =
    rtSize' pointer 0
    where rtSize' pointer size =
		do char <- mfetch pointer
		   case char of
			0 -> return $ IntRes size
			_ -> rtSize' (pointer + 4) (size + 1)

rtSize _ =
    error "size: invalid arguments"

rtConcat :: [Res] -> Mnd Res
rtConcat [r1@(IntRes str1), r2@(IntRes str2)] =
    do (IntRes size1) <- rtSize [r1]
       (IntRes size2) <- rtSize [r2]
       pointer <- mreserve $ (size1 + size2 + 1) * 4
       rtConcat' pointer str1 size1
       rtConcat' (pointer + size1 * 4) str2 size2
       mstore (pointer + (size1 + size2) * 4) 0
       return $ IntRes pointer
    where rtConcat' pointer string 0 =
	      return ()

	  rtConcat' pointer string size =
	      do char <- mfetch string
		 mstore pointer char
		 rtConcat' (pointer + 4) (string + 4) (size - 1)

rtConcat _ =
    error "concat: invalid arguments"

rtCompare :: [Res] -> Mnd Res
rtCompare [IntRes str1, IntRes str2] =
    rtCompare' str1 str2
    where rtCompare' str1 str2 =
	      do char1 <- mfetch str1
		 char2 <- mfetch str2
		 case (char1 == 0 && char2 == 0, char1 - char2) of
		      (True, _)  -> return $ IntRes 0
		      (False, 0) -> rtCompare' (str1 + 4) (str2 + 4)
		      (False, n) -> return $ IntRes (signum n)

rtCompare _ =
    error "strcmp: invalid arguments"

rtStringEqual :: [Res] -> Mnd Res
rtStringEqual [IntRes str1, IntRes str2] =
    rtStringEqual' str1 str2
    where rtStringEqual' str1 str2 =
	      do char1 <- mfetch str1
		 char2 <- mfetch str2
		 case (char1 == 0 && char2 == 0, char1 - char2) of
		      (True, _)  -> return $ IntRes 1
		      (False, 0) -> rtStringEqual' (str1 + 4) (str2 + 4)
		      (False, n) -> return $ IntRes 0

rtStringEqual _ =
    error "streq: invalid arguments"

rtSubstring :: [Res] -> Mnd Res
rtSubstring [s@(IntRes string), IntRes from, IntRes len] =
    do (IntRes size) <- rtSize [s]
       if (not (0 <= from && 0 <= len && from + len <= size))
          then
	      do rtError "substring: arguments out of bounds"
		 return $ IntRes string
	  else
	      do pointer <- mreserve $ (len + 1) * 4
                 rtSubstring' pointer (string + from * 4) len
                 return $ IntRes pointer
    where rtSubstring' pointer string 0 =
	      do mstore pointer 0
		 return ()

          rtSubstring' pointer string len =
	      do char <- mfetch string
		 mstore pointer char
		 rtSubstring' (pointer + 4) (string + 4) (len - 1)

rtSubstring _ =
    error "substring: invalid arguments"
