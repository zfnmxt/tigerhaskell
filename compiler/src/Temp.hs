{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Temp where

import Control.Monad.Trans.State.Lazy (StateT, State)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Trans.State.Lazy as S

data Temp      = Temp Int  deriving (Eq, Show)
data Label     = Label Int deriving (Eq, Show)
type GenTempT  = StateT Temp
type GenTemp   = GenTempT Identity

--nextRTemp :: Monad m => GenTempT m RTemp
--nextRTemp = do
--   temp@Temp{..} <- S.get
--   S.put  $ temp { _tempRTempC = _tempRTempC + 1}
--   return $ RTemp _tempRTempC
--
--nextLabel :: Monad m => GenTempT m Label
--nextLabel = do
--   temp@Temp{..} <- S.get
--   S.put  $ temp { _tempLabelC = _tempLabelC + 1}
--   return $ Label _tempLabelC $ "L" ++ show _tempLabelC
--
--nextNamedLabel :: Monad m => String -> GenTempT m Label
--nextNamedLabel name = do
--   temp@Temp{..} <- S.get
--   S.put  $ temp { _tempLabelC = _tempLabelC + 1}
--   return $ Label _tempLabelC name
--
--class MonadTemp m where
--  mkRTemp       :: m RTemp
--  mkLabel       :: m Label
--  mkNamedLabel  :: String -> m Label

