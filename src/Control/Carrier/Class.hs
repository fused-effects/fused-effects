{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeOperators, UndecidableInstances #-}
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
import           Control.Effect.Empty
import           Control.Effect.Error
import           Control.Effect.Reader
import qualified Control.Effect.Sum as Sum
import           Control.Monad ((<=<))

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


instance Carrier Empty Maybe where
  eff Empty = Nothing

instance Carrier (Error e) (Either e) where
  eff (Throw e)     = Left e
  eff (Catch m h k) = either (k <=< h) k m

instance Carrier (Reader r) ((->) r) where
  eff (Ask       k) r = k r r
  eff (Local f m k) r = k (m (f r)) r
