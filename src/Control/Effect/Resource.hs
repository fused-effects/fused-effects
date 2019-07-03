{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resource
( Resource(..)
, bracket
, bracketOnError
, finally
, onException
, runResource
, ResourceC(..)
) where

import           Control.Applicative (Alternative(..))
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Effect.Sum
import qualified Control.Exception as Exc
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

data Resource m k
  = forall resource any output . Resource (m resource) (resource -> m any) (resource -> m output) (output -> k)
  | forall resource any output . OnError  (m resource) (resource -> m any) (resource -> m output) (output -> k)

deriving instance Functor (Resource m)

instance HFunctor Resource where
  hmap f (Resource acquire release use k) = Resource (f acquire) (f . release) (f . use) k
  hmap f (OnError acquire release use k)  = OnError  (f acquire) (f . release) (f . use) k

instance Effect Resource where
  handle state handler (Resource acquire release use k) = Resource (handler (acquire <$ state)) (handler . fmap release) (handler . fmap use) (handler . fmap k)
  handle state handler (OnError acquire release use k)  = OnError  (handler (acquire <$ state)) (handler . fmap release) (handler . fmap use) (handler . fmap k)

-- | Provides a safe idiom to acquire and release resources safely.
--
-- When acquiring and operating on a resource (such as opening and
-- reading file handle with 'openFile' or writing to a blob of memory
-- with 'malloc'), any exception thrown during the operation may mean
-- that the resource is not properly released. @bracket acquire release op@
-- ensures that @release@ is run on the value returned from @acquire@ even
-- if @op@ throws an exception.
--
-- 'bracket' is safe in the presence of asynchronous exceptions.
bracket :: (Member Resource sig, Carrier sig m)
        => m resource           -- ^ computation to run first ("acquire resource")
        -> (resource -> m any)  -- ^ computation to run last ("release resource")
        -> (resource -> m a)    -- ^ computation to run in-between
        -> m a
bracket acquire release use = send (Resource acquire release use pure)

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError :: (Member Resource sig, Carrier sig m)
               => m resource           -- ^ computation to run first ("acquire resource")
               -> (resource -> m any)  -- ^ computation to run last ("release resource")
               -> (resource -> m a)    -- ^ computation to run in-between
               -> m a
bracketOnError acquire release use = send (OnError acquire release use pure)

-- | Like 'bracket', but for the simple case of one computation to run afterward.
finally :: (Member Resource sig, Carrier sig m)
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally act end = bracket (pure ()) (const end) (const act)

-- | Like 'bracketOnError', but for the simple case of one computation to run afterward.
onException :: (Member Resource sig, Carrier sig m)
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward if an exception was raised
        -> m a
onException act end = bracketOnError (pure ()) (const end) (const act)

-- Not exposed due to its potential to silently drop effects (#180).
unliftResource :: (forall x . m x -> IO x) -- ^ "unlifting" function to run the carrier in 'IO'
            -> ResourceC m a
            -> m a
unliftResource handler = runReader (Handler handler) . runResourceC

-- | Executes a 'Resource' effect. Because this runs using 'MonadUnliftIO',
-- invocations of 'runResource' must happen at the "bottom" of a stack of
-- effect invocations, i.e. before the use of any monads that lack such
-- instances, such as 'StateC':
--
-- @
--   runM
--   . runResource
--   . runState @Int 1
--   $ myComputation
-- @
runResource :: MonadUnliftIO m
            => ResourceC m a
            -> m a
runResource r = withRunInIO (\f -> runHandler (Handler f) r)

newtype ResourceC m a = ResourceC { runResourceC :: ReaderC (Handler m) m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadUnliftIO m => MonadUnliftIO (ResourceC m) where
  askUnliftIO = ResourceC . ReaderC $ \(Handler h) ->
    withUnliftIO $ \u -> pure (UnliftIO $ \r -> unliftIO u (unliftResource h r))

instance MonadTrans ResourceC where
  lift = ResourceC . lift

newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> ResourceC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runResourceC

instance (Carrier sig m, MonadIO m) => Carrier (Resource :+: sig) (ResourceC m) where
  eff (L (Resource acquire release use k)) = do
    handler <- ResourceC ask
    a <- liftIO (Exc.bracket
      (runHandler handler acquire)
      (runHandler handler . release)
      (runHandler handler . use))
    k a
  eff (L (OnError  acquire release use k)) = do
    handler <- ResourceC ask
    a <- liftIO (Exc.bracketOnError
      (runHandler handler acquire)
      (runHandler handler . release)
      (runHandler handler . use))
    k a
  eff (R other)                            = ResourceC (eff (R (handleCoercible other)))
