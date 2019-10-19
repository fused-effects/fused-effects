{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

-- | A carrier for an 'Error' effect.
module Control.Carrier.Error.Either
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module X
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.Error
import Control.Effect.Error as X (Error, Throw, Catch)
import Control.Effect.Error as X hiding (Error, Throw, Catch)
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
-- @
-- 'runError' ('pure' a) = 'pure' ('Right' a)
-- @
-- @
-- 'runError' ('throwError' e) = 'pure' ('Left' e)
-- @
-- @
-- 'runError' ('throwError' e `catchError` 'pure') = 'pure' ('Right' e)
-- @
--
-- @since 0.1.0.0
runError :: ErrorC exc m a -> m (Either exc a)
runError = runExceptT . runErrorC

newtype ErrorC e m a = ErrorC { runErrorC :: ExceptT e m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadTrans)

-- | 'ErrorC' passes 'Alternative' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => Alternative (ErrorC e m) where
  empty = ErrorC (ExceptT empty)
  {-# INLINE empty #-}
  ErrorC (ExceptT l) <|> ErrorC (ExceptT r) = ErrorC (ExceptT (l <|> r))
  {-# INLINE (<|>) #-}

-- | 'ErrorC' passes 'MonadPlus' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (ErrorC e m) where
  eff = ErrorC . eff . handleCoercible
  {-# INLINE eff #-}
