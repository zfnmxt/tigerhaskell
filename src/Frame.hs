{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Frame
  ( Escape,
    Frame (..),
    X86,
  )
where

import Data.Proxy
import Temp qualified
import Tree

type Escape = Bool

class Frame frame where
  data Access frame :: *
  newFrame :: Temp.Label -> [Escape] -> frame
  name :: frame -> Temp.Label
  formals :: frame -> [Access frame]
  allocLocal :: frame -> Escape -> Access frame
  fP :: Proxy frame -> Temp.Temp
  wordSize :: Proxy frame -> Integer
  exp :: Access frame -> Tree.Exp -> Tree.Exp
  staticLink :: frame -> Tree.Exp

data X86

instance Frame X86
