{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

{- | Provides 'NonDetC', a carrier for 'NonDet' effects providing choice and failure.

Under the hood, it uses a Church-encoded structure and a binary tree to prevent the problems associated with a naïve list-based implementation.
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

import Control.Applicative (liftA2)
import Control.Carrier
import Control.Effect.NonDet
import Control.Monad (join)
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
--   prop> run (runNonDetA (pure a)) === [a]
--   prop> run (runNonDetA (pure a)) === Just a
--
-- @since 1.0.0.0
runNonDetA :: (Alternative f, Applicative m) => NonDetC m a -> m (f a)
runNonDetA = runNonDet (liftA2 (<|>)) (pure . pure) (pure empty)

-- | Run a 'NonDet' effect, collecting results into a 'Monoid'.
--
-- @since 1.0.0.0
runNonDetM :: (Applicative m, Monoid b) => (a -> b) -> NonDetC m a -> m b
runNonDetM leaf = runNonDet (liftA2 mappend) (pure . leaf) (pure mempty)

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
--
-- @since 1.0.0.0
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving three continuations, respectively implementing '<|>', 'pure', and 'empty'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b
  }
  deriving (Functor)

-- $
--   prop> run (runNonDetA (pure a *> pure b)) === Just b
--   prop> run (runNonDetA (pure a <* pure b)) === Just a
instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ leaf _ -> leaf a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

-- $
--   prop> run (runNonDetA (pure a <|> (pure b <|> pure c))) === Fork (Leaf a) (Fork (Leaf b) (Leaf c))
--   prop> run (runNonDetA ((pure a <|> pure b) <|> pure c)) === Fork (Fork (Leaf a) (Leaf b)) (Leaf c)
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
--
-- >>> run (runNonDetA @[] (take 3 <$> mfix (\ as -> pure (0 : map succ as) <|> pure (0 : map pred as))))
-- [[0,1,2],[0,-1,-2]]
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

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L (L Empty))      = empty
  eff (L (R (Choose k))) = k True <|> k False
  eff (R other)          = NonDetC $ \ fork leaf nil -> eff (handle (Leaf ()) (fmap join . traverse runNonDetA) other) >>= fold fork leaf nil
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


-- $setup
-- >>> import Test.QuickCheck
