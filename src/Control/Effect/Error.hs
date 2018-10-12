{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Error
( Error(..)
, throwError
, catchError
, runError
, ErrorC(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal
import Control.Monad ((<=<))

data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> k)

deriving instance Functor (Error exc m)

instance HFunctor (Error exc) where
  hfmap _ (Throw exc)   = Throw exc
  hfmap f (Catch m h k) = Catch (f m) (f . h) k

instance Effect (Error exc) where
  handle _     _       (Throw exc)   = Throw exc
  handle state handler (Catch m h k) = Catch (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

-- | Throw an error, escaping the current computation up to the nearest 'catchError' (if any).
--
--   prop> run (runError (throwError a)) == Left @Int @Int a
throwError :: (Member (Error exc) sig, Carrier sig m) => exc -> m a
throwError = send . Throw

-- | Run a computation which can throw errors with a handler to run on error.
--
--   Errors thrown by the handler will escape up to the nearest enclosing 'catchError' (if any).
--
--   prop> run (runError (pure a `catchError` pure)) == Right a
--   prop> run (runError (throwError a `catchError` pure)) == Right @Int @Int a
--   prop> run (runError (throwError a `catchError` (throwError @Int))) == Left @Int @Int a
catchError :: (Member (Error exc) sig, Carrier sig m) => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h gen)


-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runError (pure a)) == Right @Int @Int a
runError :: (Carrier sig m, Effect sig, Monad m) => Eff (ErrorC exc m) a -> m (Either exc a)
runError = runErrorC . interpret

newtype ErrorC e m a = ErrorC { runErrorC :: m (Either e a) }

instance (Carrier sig m, Effect sig, Monad m) => Carrier (Error e :+: sig) (ErrorC e m) where
  gen a = ErrorC (pure (Right a))
  alg = algE \/ (ErrorC . alg . handle (Right ()) (either (pure . Left) runErrorC))
    where algE (Throw e)     = ErrorC (pure (Left e))
          algE (Catch m h k) = ErrorC (runErrorC m >>= either (either (pure . Left) (runErrorC . k) <=< runErrorC . h) (runErrorC . k))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
