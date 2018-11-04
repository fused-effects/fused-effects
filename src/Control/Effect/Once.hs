{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Once where

import Control.Effect.Carrier

data Once m k
  = forall a . Once (m a) (a -> k)

deriving instance Functor (Once m)

instance HFunctor Once where
  hmap f (Once m k) = Once (f m) k
  {-# INLINE hmap #-}
