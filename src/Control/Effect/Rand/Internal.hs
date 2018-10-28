{-# LANGUAGE DeriveFunctor, ExistentialQuantification, KindSignatures, StandaloneDeriving #-}
module Control.Effect.Rand.Internal
( Rand(..)
) where

import Control.Effect.Carrier
import Data.Coerce (coerce)
import System.Random (Random(..))

data Rand (m :: * -> *) k
  = forall a . Random a => Uniform (a -> k)
  | forall a . Random a => UniformR (a, a) (a -> k)

deriving instance Functor (Rand m)

instance HFunctor Rand where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Rand where
  handle state handler (Uniform k) = Uniform (handler . (<$ state) . k)
  handle state handler (UniformR r k) = UniformR r (handler . (<$ state) . k)
