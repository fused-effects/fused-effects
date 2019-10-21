{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

-- | A carrier for 'Cut' and 'NonDet' effects used in tandem (@Cut :+: NonDet@).
--
-- @since 1.0.0.0
module Control.Carrier.Cut.Church
( -- * Cut carrier
  runCut
, runCutA
, runCutM
, CutC(..)
  -- * Cut effect
, module Control.Effect.Cut
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Applicative (liftA2)
import Control.Carrier.Class
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Effect.Cut
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Function (on)

-- | Run a 'Cut' effect with continuations respectively interpreting '<|>', 'pure', and 'empty'.
--
-- @since 1.0.0.0
runCut :: Monad m => (m b -> m b -> m b) -> (a -> m b) -> m b -> CutC m a -> m b
runCut fork leaf nil (CutC m) = evalState False (runNonDet (fmap lift . fork `on` evalState False) (lift . leaf) (lift nil) m)

-- | Run a 'Cut' effect, returning all its results in an 'Alternative' collection.
--
-- @since 1.0.0.0
runCutA :: (Alternative f, Monad m) => CutC m a -> m (f a)
runCutA = runCut (liftA2 (<|>)) (pure . pure) (pure empty)

-- | Run a 'Cut' effect, mapping results into a 'Monoid'.
--
-- @since 1.0.0.0
runCutM :: (Monad m, Monoid b) => (a -> b) -> CutC m a -> m b
runCutM leaf = runCut (liftA2 mappend) (pure . leaf) (pure mempty)

-- | @since 1.0.0.0
newtype CutC m a = CutC (NonDetC (StateC Bool m) a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadIO)

instance (Carrier sig m, Effect sig) => Alternative (CutC m) where
  empty = CutC empty
  {-# INLINE empty #-}
  CutC l <|> CutC r = CutC $ get >>= \ prune ->
    l <|> if prune then empty else r
  {-# INLINE (<|>) #-}

-- | Separate fixpoints are computed for each branch.
deriving instance MonadFix m => MonadFix (CutC m)

instance (Carrier sig m, Effect sig) => MonadPlus (CutC m)

instance MonadTrans CutC where
  lift = CutC . lift . lift
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff (L Cutfail)            = CutC (put True *> empty)
  eff (L (Call m k))         = m <* CutC (put False) >>= k
  eff (R (L (L Empty)))      = empty
  eff (R (L (R (Choose k)))) = k True <|> k False
  eff (R (R other))          = CutC (eff (R (R (handleCoercible other))))
  {-# INLINE eff #-}
