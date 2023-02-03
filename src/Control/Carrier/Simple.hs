{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Provides an alternate, simpler formulation of effects and their handlers that trades speed and power for simplicity and ergonomics.

The @SimpleC@ carrier, given some /first-order effect/ type @eff@, interprets that effect type with a higher-order function, passed the first parameter to 'interpret':

@
  interpret :: (forall x. eff x -> m x) -> SimpleC eff m a -> m a
@

This formulation allows you to use simple functions to interpret your effect—no 'Algebra' instances are required. If you're just getting started with implementing your own effect types, this is almost certainly the most user-friendly formulation of effects: a data type and an interpreter function.

__Please note__ that this formulation of effects has two notable downsides:

1. It bypasses this library's ability to fuse effect handlers through computations. The performance difference associated with using this module versus defining 'Algebra' instances will be significant; if you're writing code that runs in a tight loop, you should probably go the extra mile and define an 'Algebra' instance. Such instances provide speed boosts because GHC aggressively inlines typeclass invocations. Without an 'Algebra' instance, which fixes a carrier to one interpretation function, the function you pass to 'interpret' could vary at runtime, and the inliner is consequently defeated (even if in practice you use the same function everywhere). However, a simple effect is primarily bound by I\/O speed, the performance impact may be minimal. The only way to know for sure is to profile your code, so we enthusiastically recommend you do that!

2. Simple effect types cannot contain effectful sub-expressions as arguments. This precludes /scoped/ effects such as 'Control.Effect.Reader.local'. However, many useful effects, like 'Control.Effect.State.State' are not scoped, and thus representable as a first-order effect. (Note that the "Control.Carrier.Interpret" module provides an interface that allows for scoped effects, at the cost of a more complicated API.) Because effects that encapsulate some underlying 'State' effect are common, we provide the 'SimpleStateC' and 'interpretState' helpers, which wrap the strict state monad.

Should your performance requirements change, it is generally possible to reformulate the interpretation of an effect with 'Algebra' without breaking client code.

This module is based on (and almost entirely API-compatible with) work done at [FOSSA](https://fossa.com) and released as part of their [fossa-cli](https://github.com/fossas/fossa-cli) project. We are grateful for their contributions.

@since 1.2.0.0

= Defining effects with 'Simple'

Effect types compatible with 'Simple' take fewer type parameters than 'Algebra'-compatible types. Here is the classic teletype example. Note that we define a type synonym for @Simple TeletypeF@ so that the implementation details of this effect do not leak into clients.

@
  data TeletypeF a where
    Read :: TeletypeF String
    Write :: String -> TeletypeF ()

  type Teletype = Simple TeletypeF
@

Whereas an 'Algebra'-compatible @TeletypeF@ effect would take an @m@ monadic parameter and @k@ continuation parameter, those are elided in the simple formulation, as this formulation is intended to hide the details of monads and their associated state parameters.

Effects are constructed by wrapping these effect types with 'sendSimple':

@
  read :: Has Teletype sig m => m String
  read = sendSimple Read

  write :: Has Teletype sig m => String -> m ()
  write = sendSimple . Write
@

Defining an interpretation function is easy:

@
  runTeletypeIO :: MonadIO m => TeletypeF a -> m a
  runTeletypeIO t = liftIO $ case t of
    Read -> getLine
    Write s -> putStrLn s
@

-}

module Control.Carrier.Simple
( -- * Simple carrier
  interpret
, SimpleC(..)
, HandlerFor(..)
  -- * Stateful interpretation
, interpretState
, SimpleStateC
, module Control.Effect.Simple
) where

import Control.Algebra
import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class
import Control.Effect.Simple

-- | Run a 'SimpleC' carrier wrapping an effect type @eff@ with a function that interprets values of type @eff x@ into values associated with some underlying monad @m@.
interpret :: (forall x. eff x -> m x) -> SimpleC eff m a -> m a
interpret f = runReader (HandlerFor f) . runSimpleC

newtype SimpleC eff m a = SimpleC { runSimpleC :: ReaderC (HandlerFor eff m) m a }
  deriving (Applicative, Alternative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadUnliftIO)

instance MonadTrans (SimpleC eff) where
  lift = SimpleC . lift

-- | An existential wrapper for an effect handler function (a natural transformation between @eff@ and @m@).
data HandlerFor eff m = HandlerFor (forall a. eff a -> m a)

-- | A type alias providing a convenient interface to simple interpreters over strictly-evaluated state values.
--
-- In practice, many useful effects are wrappers over 'Control.Effect.State.State' effects invoked with 'StateC'.
type SimpleStateC s eff m = SimpleC eff (StateC s m)

-- | A wrapper function over 'interpret' that admits a function returning a 'StateC' carrier.
interpretState :: s -> (forall a. eff a -> StateC s m a) -> SimpleC eff (StateC s m) b -> m (s, b)
interpretState s f = runState s . interpret f

instance Algebra sig m => Algebra ((Simple eff) :+: sig) (SimpleC eff m) where
  alg hdl sig ctx = SimpleC $ do
    case sig of
      L (Simple ours) -> ReaderC $ \(HandlerFor g) -> do
        res <- g ours
        pure (res <$ ctx)
      R other -> alg (runSimpleC . hdl) (R other) ctx
