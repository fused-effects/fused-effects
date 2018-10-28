{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Random.Internal
( Random(..)
) where

import Control.Effect.Carrier
import qualified System.Random as R (Random(..))

data Random m k
  = forall a . R.Random a => Random (a -> k)
  | forall a . R.Random a => RandomR (a, a) (a -> k)
  | forall a . Interleave (m a) (a -> k)

deriving instance Functor (Random m)

instance HFunctor Random where
  hmap _ (Random k) = Random k
  hmap _ (RandomR r k) = RandomR r k
  hmap f (Interleave m k) = Interleave (f m) k
  {-# INLINE hmap #-}

instance Effect Random where
  handle state handler (Random    k) = Random    (handler . (<$ state) . k)
  handle state handler (RandomR r k) = RandomR r (handler . (<$ state) . k)
  handle state handler (Interleave m k) = Interleave (handler (m <$ state)) (handler . fmap k)
