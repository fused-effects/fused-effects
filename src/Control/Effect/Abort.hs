{-# LANGUAGE DeriveFunctor, FlexibleContexts, KindSignatures #-}
module Control.Effect.Abort
( -- * Abort effect
  Abort(..)
, abort
) where

import Control.Effect.Carrier

data Abort (m :: * -> *) k = Abort
  deriving (Functor)

abort :: (Carrier sig m, Member Abort sig) => m a
abort = send Abort
