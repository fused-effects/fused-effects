{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( -- * NonDet effect
  NonDet(..)
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Alternative(..)
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Monad (MonadPlus(..), join)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.BinaryTree as BinaryTree
import Data.Maybe (fromJust)
import GHC.Generics (Generic1)
import Prelude hiding (fail)

data NonDet m k
  = Empty
  | Choose (Bool -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)


-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using @[]@ as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'Control.Effect.Cull.runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) === [a]
--   prop> run (runNonDet (pure a)) === Just a
runNonDet :: (Alternative f, Applicative m) => NonDetC m a -> m (f a)
runNonDet (NonDetC m) = m (liftA2 (<|>)) (pure . pure) (pure empty)

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving three continuations, respectively implementing '<|>',  'pure', and 'empty'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b
  }
  deriving stock (Functor)

-- $
--   prop> run (runNonDet (pure a *> pure b)) === Just b
--   prop> run (runNonDet (pure a <* pure b)) === Just a
instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ pure _ -> pure a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

-- $
--   prop> run (runNonDet (pure a <|> (pure b <|> pure c))) === Fork (Leaf a) (Fork (Leaf b) (Leaf c))
--   prop> run (runNonDet ((pure a <|> pure b) <|> pure c)) === Fork (Fork (Leaf a) (Leaf b)) (Leaf c)
instance Alternative (NonDetC m) where
  empty = NonDetC (\ _ _ empty -> empty)
  {-# INLINE empty #-}
  NonDetC l <|> NonDetC r = NonDetC $ \ fork leaf nil -> fork (l fork leaf nil) (r fork leaf nil)
  {-# INLINE (<|>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf nil ->
    a fork (\ a' -> runNonDetC (f a') fork leaf nil) nil
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (NonDetC m) where
  fail s = lift (fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (NonDetC m) where
  mfix f = NonDetC $ \ fork leaf nil ->
    mfix (\ a -> runNonDetC (f (fromJust (BinaryTree.fold (<|>) Just Nothing a)))
      (liftA2 Fork)
      (pure . Leaf)
      (pure Nil))
    >>= BinaryTree.fold fork leaf nil
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadPlus (NonDetC m)

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf _ -> m >>= leaf)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L Empty)      = empty
  eff (L (Choose k)) = k True <|> k False
  eff (R other)      = NonDetC $ \ fork leaf nil -> eff (handle (Leaf ()) (fmap join . traverse runNonDet) other) >>= BinaryTree.fold fork leaf nil
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Foldable (asum)
