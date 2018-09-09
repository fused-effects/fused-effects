{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect where

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) (Eff effects a))

data Void m a


data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)

data NonDet m a
  = Empty
  | Choose (m a) (m a)
  deriving (Eq, Ord, Show)
