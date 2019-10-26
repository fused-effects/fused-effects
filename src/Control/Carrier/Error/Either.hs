{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A carrier for an 'Error' effect.
--
-- @since 1.0.0.0
module Control.Carrier.Error.Either
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Error
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Coerce

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
runError (ErrorC m) = runExceptT m

-- | @since 0.1.0.0
newtype ErrorC e m a = ErrorC (ExceptT e m a)
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadTrans)

-- | 'ErrorC' passes 'Alternative' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => Alternative (ErrorC e m) where
  empty = ErrorC (ExceptT empty)
  {-# INLINE empty #-}
  ErrorC (ExceptT l) <|> ErrorC (ExceptT r) = ErrorC (ExceptT (l <|> r))
  {-# INLINE (<|>) #-}

-- | 'ErrorC' passes 'MonadPlus' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTransContext (Either e) (ErrorC e) where
  liftHandle handle = ErrorC (liftHandle (\ ctx dst -> handle ctx (dst . coerce)))

instance (Algebra sig m, CanThread sig (Either e)) => Algebra (Error e :+: sig) (ErrorC e m) where
  -- NB: 'send' (& thus 'handleCoercible') can’t send sums, so we decompose the sum manually.
  alg (L (L op)) = ErrorC (handleCoercible op)
  alg (L (R op)) = ErrorC (handleCoercible op)
  alg (R op)     = handling op
  {-# INLINE alg #-}
