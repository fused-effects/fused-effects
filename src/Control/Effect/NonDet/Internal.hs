{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Control.Effect.NonDet.Internal
( NonDet(..)
) where

import Control.Effect.Carrier
import Data.Coerce

data NonDet (m :: * -> *) k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance HFunctor NonDet where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect NonDet where
  handle _     _       Empty      = Empty
  handle state handler (Choose k) = Choose (handler . (<$ state) . k)
