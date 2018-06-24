module Print
where

import Data.Char (chr)

import Ir

indent :: Int
indent = 4

instance (Show a) => Show (Exp a) where
    show exp = showExp 0 exp
    showList exps = showString $ showExps 0 exps

instance (Show a) => Show (Stm a) where
    show stm = showStm 0 stm
    showList stms = showString $ showStms 0 stms

showExps :: Show a => Int -> [Exp a] -> String
showExps n exps = unlines (map (showExp n) exps)

showExp :: Show a => Int -> Exp a -> String
showExp n (Const ann i) =
    replicate n ' '
    ++ show ann
    ++ unwords ["const", show i]

showExp n (Name ann s) =
    replicate n ' '
    ++ show ann
    ++ unwords ["name", s]

showExp n (Temp ann s) =
    replicate n ' '
    ++ show ann
    ++ unwords ["temp", s]

showExp n (Binop ann op left right) =
    replicate n ' '
    ++ show ann
    ++ unwords ["binop", show op]
    ++ "\n"
    ++ showExps (n + indent) [left, right]

showExp n (Mem ann e) =
    replicate n ' '
    ++ show ann
    ++ "mem\n"
    ++ showExp (n + indent) e

showExp n (Call ann f exps) =
    replicate n ' '
    ++ show ann
    ++ "call\n"
    ++ showExps (n + indent) (f : exps)
    ++ replicate n ' '
    ++ show ann
    ++ unwords ["call", "end"]

showExp n (ESeq ann s e) =
    replicate n ' '
    ++ show ann
    ++ "eseq\n"
    ++ showStm (n + indent) s
    ++ "\n"
    ++ showExp (n + indent) e

showStms :: Show a => Int -> [Stm a] -> String
showStms n stms = unlines (map (showStm n) stms)

showStm :: Show a => Int -> Stm a -> String
showStm n (Move ann e f) =
    replicate n ' '
    ++ show ann
    ++ "move"
    ++ "\n"
    ++ showExps (n + indent) [e, f]

showStm n (Sxp ann e) =
    replicate n ' '
    ++ show ann
    ++ "sxp"
    ++ "\n"
    ++ showExp (n + indent) e

showStm n (Jump ann e) =
    replicate n ' '
    ++ show ann
    ++ "jump\n"
    ++ showExp (n + indent) e

showStm n (CJump ann op left right true false) =
    replicate n ' '
    ++ show ann
    ++ unwords ["cjump", show op]
    ++ "\n"
    ++ showExps (n + indent) [left, right, true, false]

showStm n (Seq ann stms) =
    replicate n ' '
    ++ show ann
    ++ "seq\n"
    ++ showStms (n + indent) stms
    ++ replicate n ' '
    ++ show ann
    ++ unwords ["seq", "end"]

showStm n (Label ann s) =
    replicate n ' '
    ++ show ann
    ++ unwords ["label", s]

showStm n (LabelEnd ann) =
    replicate n ' '
    ++ show ann
    ++ unwords ["label", "end"]

showStm n (Literal ann name content) =
    replicate n ' '
    ++ show ann
    ++ "\n"
    ++ unwords [name, map chr content]
