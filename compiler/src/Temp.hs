module Temp (
   Temp (..)
 , Label
 , initTemp
 , mkTemp
 , mkLabel
 , initLabel
 , mkNamedLabel
) where

import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as S

data Temp     = Outermost | Temp Int
data Label    = Label {_labelNum :: Int, _labelName :: String}
type GenTemp  = State Temp
type GenLabel = State Int

initTemp :: GenTemp ()
initTemp = S.put Outermost

mkTemp :: GenTemp Temp
mkTemp = do
   temp <- S.get
   case temp of
     Outermost -> do
       S.put  $ Temp 1
       return $ Temp 1
     Temp x    -> do
       S.put  $ Temp (x + 1)
       return $ Temp (x + 1)

initLabel :: GenLabel ()
initLabel = S.put 0

mkLabel :: GenLabel Label
mkLabel = do
  labelNum <- S.get
  S.put $ labelNum + 1
  return $ Label {_labelNum = labelNum, _labelName = "L" ++ show labelNum}

mkNamedLabel :: String -> GenLabel Label
mkNamedLabel name = do
 label <- mkLabel
 return $ label {_labelName = name}







