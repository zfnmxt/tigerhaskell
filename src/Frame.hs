{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Frame
  ( Escape,
    Frame (..),
    X86,
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

data X86

instance Frame X86
