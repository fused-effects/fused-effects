{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
{- |
A carrier for 'Cull' and 'NonDet' effects used in tandem (@Cull :+: NonDet@).
-}

module Control.Carrier.Cull.Church
( -- * Cull effect
  module Control.Effect.Cull
  -- * NonDet effects
, module Control.Effect.NonDet
  -- * Cull carrier
, runCull
, runCullA
, runCullM
, CullC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Applicative (liftA2)
import Control.Carrier
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Effect.Cull
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cull' effect with the supplied continuations for '<|>', 'pure', and 'empty'. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runCull (liftA2 (<|>)) (pure . pure) (pure empty) (pure a <|> pure b)) === [a, b]
--
-- @since 1.0.0.0
runCull :: (m b -> m b -> m b) -> (a -> m b) -> m b -> CullC m a -> m b
runCull fork leaf nil = runNonDet fork leaf nil . runReader False . runCullC

-- | Run a 'Cull' effect, interpreting the result into an 'Alternative' functor. Choice is handled with '<|>', embedding with 'pure', and failure with 'Control.Applicative.empty'.
--
-- @since 1.0.0.0
runCullA :: (Alternative f, Applicative m) => CullC m a -> m (f a)
runCullA = runCull (liftA2 (<|>)) (pure . pure) (pure empty)

-- | Run a 'Cull' effect, interpreting the result into a 'Monoid'. Choice is handled with 'mappend', failure with 'empty', and embedding with the composition of 'pure' and the provided function returning a monoid.
--
-- @since 1.0.0.0
runCullM :: (Applicative m, Monoid b) => (a -> b) -> CullC m a -> m b
runCullM leaf = runCull (liftA2 mappend) (pure . leaf) (pure mempty)

newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}
  l <|> r = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReader cull (runCullC l)) fork leaf (runNonDetC (runReader cull (runCullC r)) fork leaf nil)
    else
      runReader cull (runCullC l) <|> runReader cull (runCullC r)
  {-# INLINE (<|>) #-}

-- | Separate fixpoints are computed for each branch.
--
-- >>> run (runCullA @[] (take 3 <$> mfix (\ as -> pure (0 : map succ as) <|> pure (0 : map pred as))))
-- [[0,1,2],[0,-1,-2]]
deriving instance MonadFix m => MonadFix (CullC m)

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  eff (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  eff (R (L (L Empty)))      = empty
  eff (R (L (R (Choose k)))) = k True <|> k False
  eff (R (R other))          = CullC (eff (R (R (handleCoercible other))))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
