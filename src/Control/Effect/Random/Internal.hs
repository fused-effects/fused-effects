{-# LANGUAGE DeriveFunctor, ExistentialQuantification, KindSignatures, StandaloneDeriving #-}
module Control.Effect.Random.Internal
( Random(..)
) where

import Control.Effect.Carrier
import Data.Coerce (coerce)
import qualified System.Random as R (Random(..))

data Random (m :: * -> *) k
  = forall a . R.Random a => Random (a -> k)
  | forall a . R.Random a => RandomR (a, a) (a -> k)

deriving instance Functor (Random m)

instance HFunctor Random where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Random where
  handle state handler (Random    k) = Random    (handler . (<$ state) . k)
  handle state handler (RandomR r k) = RandomR r (handler . (<$ state) . k)
