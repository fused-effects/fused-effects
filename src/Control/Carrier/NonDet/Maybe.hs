{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.NonDet.Maybe
( -- * NonDet effects
  module Control.Effect.NonDet
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier
import Control.Effect.NonDet
import Control.Monad (MonadPlus (..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

runNonDet :: NonDetC m a -> m (Maybe a)
runNonDet = runMaybeT . runNonDetC

newtype NonDetC m a = NonDetC { runNonDetC :: MaybeT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L (L Empty))      = empty
  eff (L (R (Choose k))) = k True <|> k False
  eff (R other) = NonDetC (MaybeT (eff (handle (Just ()) (maybe (pure Nothing) runNonDet) other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
