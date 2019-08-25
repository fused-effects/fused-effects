{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, RankNTypes #-}
module Control.Effect.NonDet.NonEmpty
( -- * NonDet effect
  NonDet(..)
, choose
  -- * NonDet carrier
, NonDetC(..)
) where

import Control.Effect.Carrier
import Control.Monad.Fail
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

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}
