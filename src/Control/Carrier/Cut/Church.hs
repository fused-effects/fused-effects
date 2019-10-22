{-# LANGUAGE DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

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

import Control.Algebra
import Control.Applicative (liftA2)
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Effect.Cut
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)

-- | Run a 'Cut' effect with continuations respectively interpreting '<|>', 'pure', and 'empty'.
--
-- @since 1.0.0.0
runCut :: Monad m => (m b -> m b -> m b) -> (a -> m b) -> m b -> CutC m a -> m b
runCut fork leaf nil (CutC m) = evalState False (runNonDet
  (\ l r -> StateC $ \ prune -> do
    (prune', l') <- runState prune l
    if prune' then
      pure (prune', l')
    else do
      (prune'', r') <- runState prune' r
      (,) prune'' <$> (pure l' `fork` pure r'))
  (lift . leaf)
  (lift nil)
  m)

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

-- | A carrier for nondeterminism with committed choice. Note that the semantics of committed choice are provided by 'runCut', so care should be taken not to subvert them by incautious manipulation of the underlying 'NonDetC'.
--
-- @since 1.0.0.0
newtype CutC m a = CutC (NonDetC (StateC Bool m) a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadIO, MonadPlus)

-- | A single fixpoint is shared between all branches.
instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC $ NonDetC $ \ fork leaf nil ->
    mfix (runNonDet
      (liftA2 Fork)
      (pure . Leaf)
      (pure Nil)
      . out . f . fromJust . fold (<|>) Just Nothing)
    >>= fold fork leaf nil where
    out (CutC m) = m

instance MonadTrans CutC where
  lift = CutC . lift . lift
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Cut :+: NonDet :+: sig) (CutC m) where
  eff (L Cutfail)            = CutC (put True *> empty)
  eff (L (Call (CutC m) k))  = CutC (NonDetC (\ fork leaf nil -> StateC $ \ prune -> (,) prune <$> evalState prune (runNonDet fork leaf nil m))) >>= k
  eff (R (L (L Empty)))      = empty
  eff (R (L (R (Choose k)))) = k True <|> k False
  eff (R (R other))          = CutC (eff (R (R (handleCoercible other))))
  {-# INLINE eff #-}


data BinaryTree a = Nil | Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  f <*> a = fold Fork (<$> a) Nil f
  {-# INLINE (<*>) #-}

instance Alternative BinaryTree where
  empty = Nil
  {-# INLINE empty #-}
  (<|>) = Fork
  {-# INLINE (<|>) #-}

instance Monad BinaryTree where
  a >>= f = fold Fork f Nil a
  {-# INLINE (>>=) #-}


fold :: (b -> b -> b) -> (a -> b) -> b -> BinaryTree a -> b
fold fork leaf nil = go where
  go Nil        = nil
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
{-# INLINE fold #-}
