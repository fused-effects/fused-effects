{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Resource
( -- * Resource effect
  module Control.Effect.Resource
  -- * Resource carrier
, runResource
, ResourceC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Class
import           Control.Carrier.Reader
import           Control.Effect.Resource
import qualified Control.Exception as Exc
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

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
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

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
