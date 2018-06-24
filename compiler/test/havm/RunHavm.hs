module RunHavm where

import Tree
import Temp
import Data.List (intercalate)
import Havm
import System.IO.Silently (capture)

_INDENT = 2

runHavm :: String -> IO String
runHavm s = fst <$> capture (havmString s)

indentN :: Int -> String
indentN n = replicate n ' '

genHavmExp :: Int -> TreeExp -> String
genHavmExp i (Const x)      = indentN i ++ "const " ++ show x
genHavmExp i (Name label)   = indentN i ++ "name "  ++ pLabel label
genHavmExp i (IReg r)       = indentN i ++ "temp "  ++ pTemp r
genHavmExp i (BinOp op l r) =
  indentN i ++ "binop " ++ pOp
  ++ "\n" ++ genHavmExp (i + _INDENT) l
  ++ "\n" ++ genHavmExp (i + _INDENT) r
  where pOp = case op of
                Plus -> "add"
                Sub  -> "sub"
                Mul  -> "mul"
                Div  -> "div"
genHavmExp i (Mem e)       = indentN i ++ "mem\n" ++ genHavmExp (i + _INDENT) e
genHavmExp i (Call e args) =
  indentN i ++ "call"
  ++ "\n" ++ genHavmExp (i + _INDENT) e
  ++ "\n" ++ intercalate "\n" (map f args)
  ++ "\n" ++ "end call"
  where f arg = genHavmExp (i + _INDENT) arg

genHavmExp i (ESeq s e) =
  indentN i ++ "eseq" 
  ++ "\n" ++ genHavmStm (i + _INDENT) s
  ++ "\n" ++ genHavmExp (i + _INDENT) e

genHavmStm :: Int -> TreeStm -> String
genHavmStm i (Move d e) =
  indentN i ++ "move"
  ++ "\n" ++ genHavmExp (i + _INDENT) d
  ++ "\n" ++ genHavmExp (i + _INDENT) e
genHavmStm i (StmExp e) =
  indentN i ++ "sxp"
  ++ "\n" ++ genHavmExp (i + _INDENT) e
genHavmStm i (Jump e [label]) = 
  indentN i ++ "jump"
  ++ "\n" ++ genHavmExp (i + _INDENT) e
  ++ "\n" ++ pLabel label
genHavmStm i (CJump op l r t f) =
  indentN i ++ "cjump " ++ pOp
  ++ "\n" ++ genHavmExp (i + _INDENT) l ++ "  #left"
  ++ "\n" ++ genHavmExp (i + _INDENT) r ++ "  #right"
  ++ "\n" ++ pLabel t                   ++ "  #true"
  ++ "\n" ++ pLabel f                   ++ "  #false"
  where pOp = case op of
                Equal  -> "eq"
                NEqual -> "ne"
                Lt     -> "lt"
                Gt     -> "gt"
                LTE    -> "le"
                GTE    -> "ge"
genHavmStm i (Seq l r) =
  indentN i ++ "seq"
  ++ "\n" ++ genHavmStm (i + _INDENT) l
  ++ "\n" ++ genHavmStm (i + _INDENT) r
  ++ "\n" ++ indentN i ++ "seq end"

--runHavm :: String -> IO String
--runHavm s = do
  

