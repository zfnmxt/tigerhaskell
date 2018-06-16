module Symbol (
    Symbol
  , initSym
  , GenSym
) where


import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S
import Data.List (replicate)

type Symbol = String
type GenSym = State Symbol

nextSym :: Symbol -> Symbol
nextSym xs         = f . reverse $ xs
  where f []       = replicate (length xs + 1) 'a'
        f ('z':ys) = f ys
        f (y:ys)   = (succ y):ys

mkSym :: GenSym Symbol
mkSym = do
  s <- S.get
  let s' = nextSym s
  S.put s'
  return s

initSym :: GenSym ()
initSym = S.put "a"
