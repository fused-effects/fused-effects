{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes #-}
module Control.Carrier.Class
( Carrier(..)
, send
-- * Re-exports
, module Control.Effect.Class
, Pure.run
, (Sum.:+:)(..)
, Sum.Member(..)
) where

import qualified Control.Carrier.Pure as Pure
import           Control.Effect.Class
import qualified Control.Effect.Sum as Sum

class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  eff :: sig m a -> m a


instance Carrier Pure.Pure Pure.PureC

send :: (Sum.Member effect sig, Carrier sig m) => effect m a -> m a
