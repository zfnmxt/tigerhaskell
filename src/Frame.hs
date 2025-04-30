{-# LANGUAGE FunctionalDependencies #-}

module Frame (Frame (..)) where

import Temp

class Frame frame access | frame -> access where
  newFrame :: Label -> [Bool] -> frame
  name :: frame -> Label
  formals :: frame -> [access]
  allocLocal :: frame -> Bool -> access

-- class MonadFrame frame access m where
--  newFrame :: Label -> [Bool] -> m frame
--  name :: frame ->
