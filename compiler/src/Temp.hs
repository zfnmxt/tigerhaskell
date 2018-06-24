module Temp where

import Registers

data Temp      = Temp Int 
               | RTemp Reg
               deriving (Eq, Show)
data Label     = Label Int
               | NamedLabel String
               deriving (Eq, Show)

pLabel :: Label -> String
pLabel (Label x)      = "_L_" ++ show x
pLabel (NamedLabel s) = "_L_" ++ s

pTemp :: Temp -> String
pTemp (Temp x)     = "_T_" ++ show x
pTemp (RTemp RBX)  = "_R_FP"
pTemp (RTemp RAX)  = "_R_RET"
pTemp (RTemp r)    = "_R_" ++ show r
