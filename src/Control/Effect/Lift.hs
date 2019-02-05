{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
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

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: Monad m => Eff (LiftC m) a -> m a
runM = runLiftC . interpret

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
sendM :: (Member (Lift n) sig, Carrier sig m, Functor n, Applicative m) => n a -> m a
sendM = send . Lift . fmap pure

newtype LiftC m a = LiftC { runLiftC :: m a }

instance Monad m => Carrier (Lift m) (LiftC m) where
  ret = LiftC . pure
  eff = LiftC . (>>= runLiftC) . unLift
