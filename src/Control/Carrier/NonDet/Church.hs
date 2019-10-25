{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

{- | Provides 'NonDetC', a carrier for 'NonDet' effects providing choice and failure.

Under the hood, it uses a Church-encoded structure and a binary tree to prevent the problems associated with a naïve list-based implementation.

@since 1.0.0.0
-}

module Control.Carrier.NonDet.Church
( -- * NonDet carrier
  runNonDet
, runNonDetA
, runNonDetM
, NonDetC(..)
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Algebra
import Control.Applicative (liftA2)
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'NonDet' effect, using the provided functions to interpret choice, leaf results, and failure.
--
-- @since 1.0.0.0
runNonDet
  :: (m b -> m b -> m b) -- ^ Handles choice ('<|>')
  -> (a -> m b)          -- ^ Handles embedding results ('pure')
  -> m b                 -- ^ Handles failure ('empty')
  -> NonDetC m a         -- ^ A nondeterministic computation to execute
  -> m b
runNonDet fork leaf nil (NonDetC m) = m fork leaf nil

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
-- Using @[]@ as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unless used with 'Control.Effect.Cull.cull', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
-- @
-- 'runNonDetA' ('pure' a) = 'pure' [a]
-- @
-- @
-- 'runNonDetA' ('pure' a) = 'pure' ('Just' a)
-- @
--
-- @since 1.0.0.0
runNonDetA :: (Alternative f, Applicative m) => NonDetC m a -> m (f a)
runNonDetA = runNonDet (liftA2 (<|>)) (pure . pure) (pure empty)

-- | Run a 'NonDet' effect, mapping results into a 'Monoid'.
--
-- @since 1.0.0.0
runNonDetM :: (Applicative m, Monoid b) => (a -> b) -> NonDetC m a -> m b
runNonDetM leaf = runNonDet (liftA2 mappend) (pure . leaf) (pure mempty)

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
--
-- @since 1.0.0.0
newtype NonDetC m a = NonDetC (forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b)
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ leaf _ -> leaf a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

instance Alternative (NonDetC m) where
  empty = NonDetC (\ _ _ nil -> nil)
  {-# INLINE empty #-}
  NonDetC l <|> NonDetC r = NonDetC $ \ fork leaf nil -> fork (l fork leaf nil) (r fork leaf nil)
  {-# INLINE (<|>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf nil ->
    a fork (runNonDet fork leaf nil . f) nil
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

-- | Separate fixpoints are computed for each branch.
instance MonadFix m => MonadFix (NonDetC m) where
  mfix f = NonDetC $ \ fork leaf nil ->
    mfix (runNonDetA . f . head)
    >>= runNonDet fork leaf nil . foldr
      (\ a _ -> pure a <|> mfix (liftAll . fmap tail . runNonDetA . f))
      empty where
    liftAll m = NonDetC $ \ fork leaf nil -> m >>= foldr (fork . leaf) nil
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadPlus (NonDetC m)

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf _ -> m >>= leaf)
  {-# INLINE lift #-}

instance (Algebra sig m, CanThread sig (NonDetC m)) => Algebra (NonDet :+: sig) (NonDetC m) where
  alg (L (L Empty))      = empty
  alg (L (R (Choose k))) = k True <|> k False
  alg (R other)          = NonDetC $ \ fork leaf nil -> handle (pure ()) (runNonDet (liftA2 (<|>)) runNonDetA (pure empty)) other >>= runNonDet fork leaf nil
  {-# INLINE alg #-}
