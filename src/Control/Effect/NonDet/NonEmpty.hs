{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
, choose
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
) where

import Control.Applicative ((<|>), liftA2)
import Control.Effect.Carrier
import Control.Monad (join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bool (bool)
import Data.Maybe (fromJust)
import GHC.Generics (Generic1)

data NonDet m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor NonDet
instance Effect   NonDet

choose :: (Carrier sig m, Member NonDet sig) => m a -> m a -> m a
choose a b = send (Choose (bool b a))


runNonDet :: (m b -> m b -> m b) -> (a -> m b) -> NonDetC m a -> m b
runNonDet fork leaf m = runNonDetC m fork leaf

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving two continuations, respectively implementing choice and 'pure'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b
  }
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ leaf -> leaf a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf ->
    f fork (\ f' -> a fork (leaf . f'))
  {-# INLINE (<*>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf ->
    a fork (\ a' -> runNonDetC (f a') fork leaf)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (NonDetC m) where
  mfix f = NonDetC $ \ fork leaf ->
    mfix (\ a -> runNonDetC (f (fromJust (fold (<|>) Just a)))
      (liftA2 Fork)
      (pure . Leaf))
    >>= fold fork leaf
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L (Choose k)) = NonDetC $ \ fork leaf -> fork (runNonDetC (k True) fork leaf) (runNonDetC (k False) fork leaf)
  eff (R other)      = NonDetC $ \ fork leaf -> eff (handle (Leaf ()) (fmap join . traverse (runNonDet (liftA2 Fork) (pure . Leaf))) other) >>= fold fork leaf
  {-# INLINE eff #-}


data BinaryTree a = Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  Leaf f   <*> a = fmap f a
  Fork a b <*> c = Fork (a <*> c) (b <*> c)

instance Monad BinaryTree where
  Leaf a   >>= f = f a
  Fork a b >>= f = Fork (a >>= f) (b >>= f)


fold :: (b -> b -> b) -> (a -> b) -> BinaryTree a -> b
fold fork leaf = go where
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
