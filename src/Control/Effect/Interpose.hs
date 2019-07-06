{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}

{- |
This module provides an 'InterposeC' carrier capable of "eavesdropping" on requests
made to other carriers. This is a useful capability for dynamism in deeply-nested
effect stacks, but can lead to complicated control flow. Be careful.
-}
module Control.Effect.Interpose
  ( InterposeC (..)
  , runInterpose
  ) where

import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Monad.Trans.Class

-- | 'runInterpose' takes a handler for a given effect (such as 'State' or 'Reader')
-- and runs that handler whenever an effect of that type is encountered. Within a
-- handler you can use all the capabilities of the underlying monad stack, including
-- the intercepted effect, and you can pass the effect on to the original handler
-- using 'send'.
--
--   prop> run . evalState @Int a . runInterpose @(State Int) (\op -> modify @Int (+b) *> send op) $ modify @Int (+b) == a + b + b
--
runInterpose :: (forall x . eff m x -> m x) -> InterposeC eff m a -> m a
runInterpose handler = runReader (Handler handler) . runInterposeC

newtype InterposeC eff m a = InterposeC { runInterposeC :: ReaderC (Handler eff m) m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans (InterposeC eff) where
  lift = InterposeC . lift

newtype Handler eff m = Handler (forall x . eff m x -> m x)

runHandler :: (HFunctor eff, Functor m) => Handler eff m -> eff (ReaderC (Handler eff m) m) a -> m a
runHandler h@(Handler handler) = handler . handlePure (runReader h)

instance (HFunctor eff, Carrier sig m, Member eff sig) => Carrier sig (InterposeC eff m) where
  eff (op :: sig (InterposeC eff m) a)
    | Just (op' :: eff (InterposeC eff m) a) <- prj op = do
      handler <- InterposeC ask
      lift (runHandler handler (handleCoercible op'))
    | otherwise = InterposeC (ReaderC (\ handler -> eff (handlePure (runReader handler . runInterposeC) op)))

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
