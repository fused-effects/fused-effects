{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Interpret
( runInterpret
, InterpretC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runInterpret :: (forall x . eff m (m x) -> m x) -> InterpretC eff m a -> m a
runInterpret handler = runReader (Handler handler) . runInterpretC

newtype InterpretC eff m a = InterpretC { runInterpretC :: ReaderC (Handler eff m) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans (InterpretC eff) where
  lift = InterpretC . lift

newtype Handler eff m = Handler (forall x . eff m (m x) -> m x)

runHandler :: HFunctor eff => Handler eff m -> eff (InterpretC eff m) (InterpretC eff m a) -> m a
runHandler h@(Handler handler) = handler . handlePure (runReader h . runInterpretC)

instance (HFunctor eff, Carrier sig m) => Carrier (eff :+: sig) (InterpretC eff m) where
  eff (L op) = do
    handler <- InterpretC ask
    lift (runHandler handler op)
  eff (R other) = InterpretC (eff (R (handleCoercible other)))
