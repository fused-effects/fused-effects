{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Error
( Error(..)
, throwError
, catchError
, runError
, ErrorC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except as MT
import Prelude hiding (fail)

data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> k)

deriving instance Functor (Error exc m)

instance HFunctor (Error exc) where
  hmap _ (Throw exc)   = Throw exc
  hmap f (Catch m h k) = Catch (f m) (f . h) k

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
-- Errors thrown by the handler will escape up to the nearest enclosing 'catchError' (if any).
-- Note that this effect does /not/ handle errors thrown from impure contexts such as IO,
-- nor will it handle exceptions thrown from pure code. If you need to handle IO-based errors,
-- consider if 'Control.Effect.Resource' fits your use case; if not, use 'liftIO' with
-- 'Control.Exception.try' or use 'Control.Exception.Catch' from outside the effect invocation.
--
--   prop> run (runError (pure a `catchError` pure)) == Right a
--   prop> run (runError (throwError a `catchError` pure)) == Right @Int @Int a
--   prop> run (runError (throwError a `catchError` (throwError @Int))) == Left @Int @Int a
catchError :: (Member (Error exc) sig, Carrier sig m) => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h pure)


-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runError (pure a)) == Right @Int @Int a
runError :: ErrorC exc m a -> m (Either exc a)
runError = MT.runExceptT . runErrorC

newtype ErrorC e m a = ErrorC { runErrorC :: MT.ExceptT e m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Alternative m, Monad m) => Alternative (ErrorC e m) where
  empty = ErrorC (MT.ExceptT empty)
  ErrorC (MT.ExceptT l) <|> ErrorC (MT.ExceptT r) = ErrorC (MT.ExceptT (l <|> r))

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (ErrorC e m) where
  eff = ErrorC . handleSum (MT.ExceptT . eff . handle (Right ()) (either (pure . Left) (MT.runExceptT . runErrorC))) (\case
    Throw e     -> MT.throwE e
    Catch m h k -> MT.catchE (runErrorC m) (runErrorC . h) >>= runErrorC . k)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
