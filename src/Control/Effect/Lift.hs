{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.Effect.Lift
( Lift(..)
, sendM
, runM
, LiftC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.Internal
import Control.Effect.Lift.Internal
import Control.Monad.Fail
import Control.Monad.IO.Class

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: Monad m => Eff (LiftC m) a -> m a
runM = runLiftC . interpret

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
sendM :: (Member (Lift n) sig, Carrier sig m, Functor n, Applicative m) => n a -> m a
sendM = send . Lift . fmap pure

newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance MonadIO (LiftC IO) where
  liftIO = sendM

instance Monad m => Carrier (Lift m) (LiftC m) where
  ret = pure
  eff = LiftC . (>>= runLiftC) . unLift
