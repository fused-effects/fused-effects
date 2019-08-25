{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, RankNTypes #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
, choose
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
) where

import Control.Effect.Carrier
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bool (bool)
import GHC.Generics (Generic1)
import Prelude hiding (fail)

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
  { -- | A higher-order function receiving three continuations, respectively implementing choice and 'pure'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b
  }
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ pure -> pure a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf ->
    f fork (\ f' -> a fork (leaf . f'))
  {-# INLINE (<*>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf ->
    a fork (\ a' -> runNonDetC (f a') fork leaf)
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (NonDetC m) where
  fail s = lift (fail s)
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}


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
