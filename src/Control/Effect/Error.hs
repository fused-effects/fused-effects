{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Error
( -- * Error effect
  Error(..)
, throwError
, catchError
  -- * Properties
, throwError_annihilation
, catchError_interception
  -- * Re-exports
, Has
) where

import {-# SOURCE #-} Control.Carrier

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
-- 'throwError' annihilates '>>=':
--
-- @
-- 'throwError' e '>>=' k = 'throwError' e
-- @
throwError :: Has (Error exc) sig m => exc -> m a
throwError = send . Throw

-- | Run a computation which can throw errors with a handler to run on error.
--
-- 'catchError' intercepts 'throwError':
--
-- @
-- 'throwError' e \``catchError`\` f = f e
-- @
--
-- Once consequence of this law is that errors thrown by the handler will escape up to the nearest enclosing 'catchError' (if any).
--
-- Note that this effect does /not/ handle errors thrown from impure contexts such as IO,
-- nor will it handle exceptions thrown from pure code. If you need to handle IO-based errors,
-- consider if 'Control.Effect.Resource' fits your use case; if not, use 'liftIO' with
-- 'Control.Exception.try' or use 'Control.Exception.Catch' from outside the effect invocation.
catchError :: Has (Error exc) sig m => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h pure)


-- Properties

-- | 'throwError' annihilates '>>='.
throwError_annihilation :: Has (Error e) sig m => (m b -> m b -> prop) -> e -> (a -> m b) -> prop
throwError_annihilation (===) e k = (throwError e >>= k) === throwError e

-- | 'catchError' intercepts 'throwError'.
catchError_interception :: Has (Error e) sig m => (m a -> m a -> prop) -> e -> (e -> m a) -> prop
catchError_interception (===) e f = (throwError e `catchError` f) === f e
