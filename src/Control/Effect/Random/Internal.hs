{-# LANGUAGE DeriveFunctor, ExistentialQuantification, KindSignatures, StandaloneDeriving #-}
module Control.Effect.Random.Internal
( Random(..)
) where

import Control.Effect.Carrier
import Data.Coerce (coerce)
import qualified System.Random as R (Random(..))

data Random (m :: * -> *) k
  = forall a . R.Random a => Uniform (a -> k)
  | forall a . R.Random a => UniformR (a, a) (a -> k)

deriving instance Functor (Random m)

instance HFunctor Random where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Random where
  handle state handler (Uniform k) = Uniform (handler . (<$ state) . k)
  handle state handler (UniformR r k) = UniformR r (handler . (<$ state) . k)
