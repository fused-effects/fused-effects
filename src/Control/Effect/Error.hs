{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Error
( -- * Error effect
  Error(..)
, throwError
, catchError
) where

import Control.Carrier.Class

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
throwError :: (Member (Error exc) sig, Carrier sig m) => exc -> m a
throwError = send . Throw

-- | Run a computation which can throw errors with a handler to run on error.
--
-- Errors thrown by the handler will escape up to the nearest enclosing 'catchError' (if any).
-- Note that this effect does /not/ handle errors thrown from impure contexts such as IO,
-- nor will it handle exceptions thrown from pure code. If you need to handle IO-based errors,
-- consider if 'Control.Effect.Resource' fits your use case; if not, use 'liftIO' with
-- 'Control.Exception.try' or use 'Control.Exception.Catch' from outside the effect invocation.
--
--   prop> run (runError (pure a `catchError` pure)) === Right a
--   prop> run (runError (throwError a `catchError` pure)) === Right @Int @Int a
--   prop> run (runError (throwError a `catchError` (throwError @Int))) === Left @Int @Int a
catchError :: (Member (Error exc) sig, Carrier sig m) => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h pure)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Error.Either
