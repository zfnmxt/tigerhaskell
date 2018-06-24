module StdRelop (relop)
where

import Data.Word

import Ir

unsigned op a b = (fromIntegral a :: Word) `op` (fromIntegral b :: Word)

relop :: Relop -> Int -> Int -> Bool
relop Eq = (==)
relop Ne = (/=)
relop Lt = (<)
relop Gt = (>)
relop Le = (<=)
relop Ge = (>=)
-- Unsigned integer inequalities.
relop Ult = unsigned (<)
relop Ule = unsigned (<=)
relop Ugt = unsigned (>)
relop Uge = unsigned (>=)
