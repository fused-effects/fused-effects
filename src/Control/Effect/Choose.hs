{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Choose
( -- * Choose effect
  Choose(..)
, choose
, optional
, many
, some
, some1
  -- * Choose carrier
, runChoose
, ChooseC(..)
) where

import Control.Applicative ((<|>), liftA2)
import Control.Effect.Carrier
import Control.Monad (join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import GHC.Generics (Generic1)

data Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
choose :: (Carrier sig m, Member Choose sig) => m a -> m a -> m a
choose a b = send (Choose (bool b a))

-- | Select between 'Just' the result of an operation, and 'Nothing'.
optional :: (Carrier sig m, Member Choose sig) => m a -> m (Maybe a)
optional a = choose (Just <$> a) (pure Nothing)

-- | Zero or more.
many :: (Carrier sig m, Member Choose sig) => m a -> m [a]
many a = go where go = choose ((:) <$> a <*> go) (pure [])

-- | One or more.
some :: (Carrier sig m, Member Choose sig) => m a -> m [a]
some a = (:) <$> a <*> many a

-- | One or more, returning a 'NonEmpty' list of the results.
some1 :: (Carrier sig m, Member Choose sig) => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a


runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseC m a -> m b
runChoose fork leaf m = runChooseC m fork leaf

-- | A carrier for 'Choose' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype ChooseC m a = ChooseC
  { -- | A higher-order function receiving two continuations, respectively implementing choice and 'pure'.
    runChooseC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b
  }
  deriving (Functor)

instance Applicative (ChooseC m) where
  pure a = ChooseC (\ _ leaf -> leaf a)
  {-# INLINE pure #-}
  ChooseC f <*> ChooseC a = ChooseC $ \ fork leaf ->
    f fork (\ f' -> a fork (leaf . f'))
  {-# INLINE (<*>) #-}

instance Monad (ChooseC m) where
  ChooseC a >>= f = ChooseC $ \ fork leaf ->
    a fork (runChoose fork leaf . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ChooseC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ChooseC m) where
  mfix f = ChooseC $ \ fork leaf ->
    mfix (runChoose (liftA2 Fork) (pure . Leaf)
      . f . fromJust . fold (<|>) Just)
    >>= fold fork leaf
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ChooseC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadTrans ChooseC where
  lift m = ChooseC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Choose :+: sig) (ChooseC m) where
  eff (L (Choose k)) = ChooseC $ \ fork leaf -> fork (runChooseC (k True) fork leaf) (runChooseC (k False) fork leaf)
  eff (R other)      = ChooseC $ \ fork leaf -> eff (handle (Leaf ()) (fmap join . traverse (runChoose (liftA2 Fork) (pure . Leaf))) other) >>= fold fork leaf
  {-# INLINE eff #-}


data BinaryTree a = Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  f <*> a = fold Fork (<$> a) f
  {-# INLINE (<*>) #-}

instance Monad BinaryTree where
  a >>= f = fold Fork f a
  {-# INLINE (>>=) #-}


fold :: (b -> b -> b) -> (a -> b) -> BinaryTree a -> b
fold fork leaf = go where
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
{-# INLINE fold #-}
