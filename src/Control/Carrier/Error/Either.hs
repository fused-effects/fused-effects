{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Error.Either
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, runError
, ErrorC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Effect.Error
import Control.Monad (MonadPlus(..), (<=<))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runError (pure a)) === Right @Int @Int a
runError :: ErrorC exc m a -> m (Either exc a)
runError = runExceptT . runErrorC

newtype ErrorC e m a = ErrorC { runErrorC :: ExceptT e m a }
  deriving (Applicative, Eq, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadTrans, Ord, Show)

-- | 'ErrorC' passes 'Alternative' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => Alternative (ErrorC e m) where
  empty = ErrorC (ExceptT empty)
  {-# INLINE empty #-}
  ErrorC (ExceptT l) <|> ErrorC (ExceptT r) = ErrorC (ExceptT (l <|> r))
  {-# INLINE (<|>) #-}

-- | 'ErrorC' passes 'MonadPlus' operations along to the underlying monad @m@, rather than combining errors à la 'ExceptT'.
instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (ErrorC e m) where
  eff (L (Throw e))     = ErrorC (ExceptT (pure (Left e)))
  eff (L (Catch m h k)) = ErrorC (ExceptT (runError m >>= either (either (pure . Left) (runError . k) <=< runError . h) (runError . k)))
  eff (R other)         = ErrorC (ExceptT (eff (handle (Right ()) (either (pure . Left) runError) other)))
  {-# INLINE eff #-}


-- $setup
-- >>> import Test.QuickCheck
