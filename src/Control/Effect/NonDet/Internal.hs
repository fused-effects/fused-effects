{-# LANGUAGE DeriveFunctor, PolyKinds #-}
module Control.Effect.NonDet.Internal where

import Control.Effect.Handler

data NonDet m k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance Effect NonDet where
  hfmap _ Empty      = Empty
  hfmap _ (Choose k) = Choose k

  handle _     _       Empty      = Empty
  handle state handler (Choose k) = Choose (handler . (<$ state) . k)
