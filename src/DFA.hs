module DFA where

import Data.Map (Map)
import qualified Data.Map as M

data StateType = Normal | Final deriving (Eq)

data State a b = State
  { stateNum :: Int,
    stateType :: StateType,
    transition :: a -> Int,
    construct :: [a] -> b
  }

data DFA a b = DFA
  { states :: Map Int (State a b),
    start :: Int
  }

matches :: DFA a b -> [a] -> Bool
matches dfa as = matches' (start dfa) as
  where
    matches' s [] =
      let state = (states dfa) M.! s
       in stateType state == Final
    matches' s (a : as) =
      let state = (states dfa) M.! s
          s' = transition state a
       in matches' s' as
