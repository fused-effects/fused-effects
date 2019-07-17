{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}

module Control.Effect.Interpret
( runInterpret
, runInterpretState
, InterpretC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Unsafe.Coerce (unsafeCoerce)


newtype InterpretC s (sig :: (* -> *) -> * -> *) m a =
  InterpretC { runInterpretC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)


instance MonadTrans (InterpretC s sig) where
  lift = InterpretC


newtype Handler sig m =
  Handler { runHandler :: forall s x . sig (InterpretC s sig m) x -> InterpretC s sig m x }


newtype Tagged a b =
  Tagged { unTag :: b }


class Reifies s a | s -> a where
  reflect :: Tagged s a


data Skolem


newtype Magic a r =
  Magic (Reifies Skolem a => Tagged Skolem r)


reify :: forall a r . a -> (forall s . Reifies s a => Tagged s r) -> r
reify a k =
  unsafeCoerce (Magic @a k) a


instance (HFunctor eff, HFunctor sig, Reifies s (Handler eff m), Monad m, Carrier sig m) => Carrier (eff :+: sig) (InterpretC s eff m) where
  eff (L eff) =
    runHandler (unTag (reflect @s)) eff
  eff (R other) =
    InterpretC (eff (handleCoercible other))



-- | Interpret an effect using a higher-order function.
--
--   This involves a great deal less boilerplate than defining a custom 'Carrier' instance, at the expense of somewhat less performance. It’s a reasonable starting point for new interpretations, and if more performance or flexibility is required, it’s straightforward to “graduate” by replacing the relevant 'runInterpret' handlers with specialized 'Carrier' instances for the effects.
--
--   At time of writing, a simple passthrough use of 'runInterpret' to handle a 'State' effect is about five times slower than using 'StateC' directly.
--
--   prop> run (runInterpret (\ op -> case op of { Get k -> k a ; Put _ k -> k }) get) === a
runInterpret
  :: forall eff m a.
     (HFunctor eff, Monad m)
  => (forall x . eff m x -> m x)
  -> (forall s . Reifies s (Handler eff m) => InterpretC s eff m a)
  -> m a
runInterpret f m =
  reify (Handler handler) (go m)

  where

    handler :: forall s x . eff (InterpretC s eff m) x -> InterpretC s eff m x
    handler e =
      InterpretC (f (handleCoercible e))

    go
      :: forall x s .
         InterpretC s eff m x
      -> Tagged s (m x)
    go m =
      Tagged (runInterpretC m)


-- | Interpret an effect using a higher-order function with some state variable.
--
--   This involves a great deal less boilerplate than defining a custom 'Carrier' instance, at the expense of somewhat less performance. It’s a reasonable starting point for new interpretations, and if more performance or flexibility is required, it’s straightforward to “graduate” by replacing the relevant 'runInterpretState' handlers with specialized 'Carrier' instances for the effects.
--
--   At time of writing, a simple use of 'runInterpretState' to handle a 'State' effect is about four times slower than using 'StateC' directly.
--
--   prop> run (runInterpretState (\ s op -> case op of { Get k -> runState s (k s) ; Put s' k -> runState s' k }) a get) === a
runInterpretState
  :: (HFunctor eff, Monad m)
  => (forall x . s -> eff (StateC s m) x -> m (s, x))
  -> s
  -> (forall t. Reifies t (Handler eff (StateC s m)) => InterpretC t eff (StateC s m) a)
  -> m (s, a)
runInterpretState handler state m =
  runState state $
  runInterpret
    (\e -> StateC (\s -> handler s e))
    m



-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
