{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Interpret
( runInterpret
, InterpretC(..)
, runInterpretState
, InterpretStateC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Interpret an effect using a higher-order function.
--
--   This involves a great deal less boilerplate than defining a custom 'Carrier' instance, at the expense of somewhat less performance. It’s a reasonable starting point for new interpretations, and if more performance or flexibility is required, it’s straightforward to “graduate” by replacing the relevant 'runInterpret' handlers with specialized 'Carrier' instances for the effects.
--
--   At time of writing, a simple passthrough use of 'runInterpret' to handle a 'State' effect is about five times slower than using 'StateC' directly.
--
--   prop> run (runInterpret (\ op -> case op of { Get k -> k a ; Put _ k -> k }) get) == a
runInterpret :: (forall x . eff m (m x) -> m x) -> InterpretC eff m a -> m a
runInterpret handler = runReader (Handler handler) . runInterpretC

newtype InterpretC eff m a = InterpretC { runInterpretC :: ReaderC (Handler eff m) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans (InterpretC eff) where
  lift = InterpretC . lift
  {-# INLINE lift #-}

newtype Handler eff m = Handler (forall x . eff m (m x) -> m x)

runHandler :: HFunctor eff => Handler eff m -> eff (InterpretC eff m) (InterpretC eff m a) -> m a
runHandler h@(Handler handler) = handler . handlePure (runReader h . runInterpretC)

instance (HFunctor eff, Carrier sig m) => Carrier (eff :+: sig) (InterpretC eff m) where
  eff (L op) = do
    handler <- InterpretC ask
    lift (runHandler handler op)
  eff (R other) = InterpretC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}


-- | Interpret an effect using a higher-order function with some state variable.
--
--   This involves a great deal less boilerplate than defining a custom 'Carrier' instance, at the expense of somewhat less performance. It’s a reasonable starting point for new interpretations, and if more performance or flexibility is required, it’s straightforward to “graduate” by replacing the relevant 'runInterpretState' handlers with specialized 'Carrier' instances for the effects.
--
--   At time of writing, a simple use of 'runInterpretState' to handle a 'State' effect is about four times slower than using 'StateC' directly.
--
--   prop> run (runInterpretState (\ s op -> case op of { Get k -> runState s (k s) ; Put s' k -> runState s' k }) a get) == a
runInterpretState :: (forall x . s -> eff (StateC s m) (StateC s m x) -> m (s, x)) -> s -> InterpretStateC eff s m a -> m (s, a)
runInterpretState handler state = runState state . runReader (HandlerState (\ eff -> StateC (\ s -> handler s eff))) . runInterpretStateC

newtype InterpretStateC eff s m a = InterpretStateC { runInterpretStateC :: ReaderC (HandlerState eff s m) (StateC s m) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans (InterpretStateC eff s) where
  lift = InterpretStateC . lift . lift
  {-# INLINE lift #-}

newtype HandlerState eff s m = HandlerState (forall x . eff (StateC s m) (StateC s m x) -> StateC s m x)

runHandlerState :: HFunctor eff => HandlerState eff s m -> eff (InterpretStateC eff s m) (InterpretStateC eff s m a) -> StateC s m a
runHandlerState h@(HandlerState handler) = handler . handlePure (runReader h . runInterpretStateC)

instance (HFunctor eff, Carrier sig m, Effect sig) => Carrier (eff :+: sig) (InterpretStateC eff s m) where
  eff (L op) = do
    handler <- InterpretStateC ask
    InterpretStateC (lift (runHandlerState handler op))
  eff (R other) = InterpretStateC (eff (R (R (handleCoercible other))))
  {-# INLINE eff #-}


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
