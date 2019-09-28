{-# LANGUAGE FunctionalDependencies #-}
module Control.Carrier.Class
( Carrier(..)
) where

import Control.Effect.Class

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a
