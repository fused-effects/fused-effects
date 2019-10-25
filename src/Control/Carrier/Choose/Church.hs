{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

{- | A carrier for 'Choose' effects (nondeterminism without failure).

Under the hood, it uses a Church-encoded binary tree to avoid the problems associated with a naïve list-based implementation (see ["ListT done right"](http://wiki.haskell.org/ListT_done_right)).

@since 1.0.0.0
-}

module Control.Carrier.Choose.Church
( -- * Choose carrier
  runChoose
, runChooseS
, ChooseC(..)
  -- * Choose effect
, module Control.Effect.Choose
) where

import Control.Algebra
import Control.Applicative (liftA2)
import Control.Effect.Choose
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List.NonEmpty (NonEmpty(..), head, tail)
import qualified Data.Semigroup as S
import Prelude hiding (head, tail)

-- | Run a 'Choose' effect with continuations respectively interpreting '<|>' and 'pure'.
--
-- @since 1.0.0.0
runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseC m a -> m b
runChoose fork leaf (ChooseC runChooseC) = runChooseC fork leaf

-- | Run a 'Choose' effect, mapping results into a 'S.Semigroup'.
--
-- @since 1.0.0.0
runChooseS :: (S.Semigroup b, Applicative m) => (a -> m b) -> ChooseC m a -> m b
runChooseS = runChoose (liftA2 (S.<>))

-- | A carrier for 'Choose' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
--
-- @since 1.0.0.0
newtype ChooseC m a = ChooseC (forall b . (m b -> m b -> m b) -> (a -> m b) -> m b)
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

instance (Algebra sig m, CanThread sig (ChooseC m)) => Algebra (Choose :+: sig) (ChooseC m) where
  alg (L (Choose k)) = ChooseC $ \ fork leaf -> fork (runChoose fork leaf (k True)) (runChoose fork leaf (k False))
  alg (R other)      = ChooseC $ \ fork leaf -> handle (pure ()) (runChoose (liftA2 (<|>)) (runChoose (liftA2 (<|>)) (pure . pure))) other >>= runChoose fork leaf
  {-# INLINE alg #-}
