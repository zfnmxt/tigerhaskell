{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Frame
  ( Escape,
    Frame (..),
  )
where

import Temp qualified

type Escape = Bool

class Frame frame where
  data Access frame :: *
  newFrame :: Temp.Label -> [Escape] -> frame
  name :: frame -> Temp.Label
  formals :: frame -> [Access frame]
  allocLocal :: frame -> Escape -> Access frame

-- class MonadFrame frame access m where
--  newFrame :: Label -> [Bool] -> m frame
--  name :: frame ->
