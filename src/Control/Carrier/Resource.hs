{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | Provides a carrier for a 'Resource' effect. This carrier is implemented atop 'Control.Exception.catch' from "Control.Exception" and is thus safe in the presence of asynchronous exceptions.
module Control.Carrier.Resource
( -- * Resource carrier
  runResource
, ResourceC(..)
  -- * Resource effect
, module Control.Effect.Resource
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier
import           Control.Effect.Resource
import qualified Control.Exception as Exc
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

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
--
-- @since 1.0.0.0
runResource :: ResourceC m a -> m a
runResource = runResourceC

newtype ResourceC m a = ResourceC { runResourceC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans ResourceC where
  lift = ResourceC

instance MonadUnliftIO m => MonadUnliftIO (ResourceC m) where
  withRunInIO f = ResourceC (withRunInIO (\ runInIO -> f (runInIO . runResourceC)))

instance (Carrier sig m, MonadUnliftIO m) => Carrier (Resource :+: sig) (ResourceC m) where
  eff (L (Resource acquire release use k)) = do
    handler <- askUnliftIO
    a <- liftIO (Exc.bracket
      (unliftIO handler acquire)
      (unliftIO handler . release)
      (unliftIO handler . use))
    k a
  eff (L (OnError  acquire release use k)) = do
    handler <- askUnliftIO
    a <- liftIO (Exc.bracketOnError
      (unliftIO handler acquire)
      (unliftIO handler . release)
      (unliftIO handler . use))
    k a
  eff (R other) = ResourceC (eff (handleCoercible other))
