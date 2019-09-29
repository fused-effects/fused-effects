{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Choose.Church
( -- * Choose effect
  module Control.Effect.Choose
  -- * Choose carrier
, runChoose
, runChooseS
, ChooseC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Applicative (liftA2)
import Control.Carrier
import Control.Effect.Choose
import Control.Monad (join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List.NonEmpty (NonEmpty(..), head, tail)
import qualified Data.Semigroup as S
import Prelude hiding (head, tail)

runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseC m a -> m b
runChoose fork leaf m = runChooseC m fork leaf

runChooseS :: (S.Semigroup b, Applicative m) => (a -> m b) -> ChooseC m a -> m b
runChooseS leaf = runChoose (liftA2 (S.<>)) leaf

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

-- | Separate fixpoints are computed for each branch.
--
-- >>> run (runChooseS @[[Integer]] (pure . pure) (take 3 <$> mfix (\ as -> pure (0 : map succ as) <|> pure (0 : map pred as))))
-- [[0,1,2],[0,-1,-2]]
instance MonadFix m => MonadFix (ChooseC m) where
  mfix f = ChooseC $ \ fork leaf ->
    mfix (runChooseS (pure . pure) . f . head)
    >>= \case
      a:|[] -> leaf a
      a:|_  -> leaf a `fork` runChoose fork leaf (mfix (liftAll . fmap tail . runChooseS (pure . pure) . f))
      where
    liftAll m = ChooseC $ \ fork leaf -> m >>= foldr1 fork . fmap leaf
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


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
