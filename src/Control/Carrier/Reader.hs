{-# LANGUAGE PatternSynonyms #-}

-- | A carrier for 'Reader' effects.
--
-- @since 1.0.0.0
module Control.Carrier.Reader
( -- * Reader carrier
  runReader
, ReaderC
, pattern ReaderC
  -- * Reader effect
, module Control.Effect.Reader
) where

import Control.Algebra
import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as T

-- | Run a 'Reader' effect with the passed environment value.
--
-- @
-- 'runReader' a 'ask' = 'pure' a
-- @
-- @
-- 'runReader' a ('pure' b) = 'pure' b
-- @
-- @
-- 'runReader' a ('local' f m) = 'runReader' (f a) m
-- @
--
-- @since 1.0.0.0
runReader :: r -> ReaderC r m a -> m a
runReader r (T.ReaderT runReaderC) = runReaderC r
{-# INLINE runReader #-}

-- | @since 1.0.0.0
type ReaderC = T.ReaderT

pattern ReaderC :: (r -> m a) -> ReaderC r m a
pattern ReaderC run = T.ReaderT run

{-# COMPLETE ReaderC #-}
