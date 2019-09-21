{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Class
( Carrier(..)
, send
) where

import qualified Control.Carrier.Pure as Pure
import           Control.Effect.Class
import qualified Control.Effect.Sum as Sum

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


instance Carrier Pure.Pure Pure.PureC where
  eff v = case v of {}
  {-# INLINE eff #-}


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Sum.Member effect sig, Carrier sig m) => effect m a -> m a
send = eff . Sum.inj
{-# INLINE send #-}
