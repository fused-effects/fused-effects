{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
  -- * Lift carrier
, runM
, LiftC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import GHC.Generics

newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: LiftC m a -> m a
runM = runLiftC

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
sendM :: (Member (Lift n) sig, Carrier sig m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure

newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Carrier (Lift m) (LiftC m) where
  eff = LiftC . (>>= runLiftC) . unLift

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runLiftC))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = LiftC $ withRunInIO $ \run -> inner (run . runLiftC)
  {-# INLINE withRunInIO #-}
