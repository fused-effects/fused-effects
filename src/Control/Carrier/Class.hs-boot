{-# LANGUAGE FunctionalDependencies #-}
module Control.Carrier.Class
( Carrier(..)
) where

import Control.Effect.Class

class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  eff :: sig m a -> m a
