{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Throw.Either
( -- * Throw carrier
  runThrow
, ThrowC(..)
  -- * Throw effect
, module Control.Effect.Throw
) where

import Control.Applicative (Alternative)
import Control.Carrier
import Control.Carrier.Error.Either
import Control.Effect.Throw
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Throw' effect, returning failures in 'Left' and successful computations’ results in 'Right'.
runThrow :: ThrowC e m a -> m (Either e a)
runThrow = runError . runThrowC

newtype ThrowC e m a = ThrowC { runThrowC :: ErrorC e m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => Carrier (Throw e :+: sig) (ThrowC e m) where
  eff (L (Throw e)) = ThrowC (throwError e)
  eff (R other)     = ThrowC (eff (R (handleCoercible other)))
