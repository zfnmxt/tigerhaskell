{- Implements Algorithm 3.13
   Grammar rules must be specified as "X -> XacY"
   where uppercase letters are nonterminals and lowercase are terminals.
   Each line can only contain one rule.
-}

module FirstFollow where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Control.Monad
import Control.Monad.Trans.State

data Symbol         = Terminal Char | NonTerminal Char deriving (Show, Eq, Ord)
type Nullable       = Set.Set Symbol
type FirstSet       = Set.Set Symbol
type FollowSet      = Set.Set Symbol
type First          = Map.Map Symbol FirstSet
type Follow         = Map.Map Symbol FollowSet
data Rule           = Rule Symbol [Symbol] deriving (Show, Eq)
data Env            = Env { rules     :: [Rule]
                          , nullable  :: Nullable
                          , firstSet  :: FirstSet
                          , followSet :: FollowSet
                          }

getTerminals :: [Symbol] -> [Symbol]
getTerminals xs = foldl f [] xs
  where f acc (Terminal x)      = (Terminal x):acc
        f acc (NonTerminal _) = acc

initRule :: Rule -> First
initRule (Rule _ rhs) = foldl addFirstSet Map.empty terminals
  where terminals          = getTerminals rhs
        addFirstSet m t    = Map.insert t (Set.singleton t) m

initFirst :: [Rule] -> First
initFirst rs = foldl Map.union Map.empty $ map initRule rs

derivesEmpty :: Rule -> Bool
derivesEmpty (Rule _ []) = True
derivesEmpty _           = False

nullable :: [Rule] -> Nullable
nullable rs = 











