{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}

{- | An effect modelling failure with a descriptive error message.

This effect is similar to the traditional @MonadError@ typeclass, though it allows the presence of multiple @Error@ effects in a given effect stack. It offers precise exception handling, rather than the dynamic exception hierarchy provided by the @exceptions@ package. The 'Control.Effect.Resource' effect or the @fused-effects-exceptions@ package may be more suitable for handling dynamic/impure effect handling.

Predefined carriers:

* "Control.Carrier.Error.Either.ErrorC".
* If 'Error' @e@ is the last effect in a stack, it can be interpreted directly to an 'Either' @e@.
-}

module Control.Effect.Error
( -- * Error effect
  Error(..)
, throwError
, catchError
  -- * Re-exports
, Has
) where

import {-# SOURCE #-} Control.Carrier

-- | @since 0.1.0.0
data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> m k)

deriving instance Functor m => Functor (Error exc m)

instance HFunctor (Error exc) where
  hmap _ (Throw exc)   = Throw exc
  hmap f (Catch m h k) = Catch (f m) (f . h) (f . k)

instance Effect (Error exc) where
  handle _     _       (Throw exc)   = Throw exc
  handle state handler (Catch m h k) = Catch (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

-- | Throw an error, escaping the current computation up to the nearest 'catchError' (if any).
--
--   prop> run (runError (throwError a)) === Left @Int @Int a
--
-- @since 0.1.0.0
throwError :: Has (Error exc) sig m => exc -> m a
throwError = send . Throw

-- | Run a computation which can throw errors with a handler to run on error.
--
-- Errors thrown by the handler will escape up to the nearest enclosing 'catchError' (if any).
-- Note that this effect does /not/ handle errors thrown from impure contexts such as IO,
-- nor will it handle exceptions thrown from pure code. If you need to handle IO-based errors,
-- consider if 'Control.Effect.Resource' fits your use case; if not, use 'Control.Monad.IO.Class.liftIO' with
-- 'Control.Exception.try' or use 'Control.Exception.catch' from outside the effect invocation.
--
--   prop> run (runError (pure a `catchError` pure)) === Right a
--   prop> run (runError (throwError a `catchError` pure)) === Right @Int @Int a
--   prop> run (runError (throwError a `catchError` (throwError @Int))) === Left @Int @Int a
--
-- @since 0.1.0.0
catchError :: Has (Error exc) sig m => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h pure)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Error.Either
