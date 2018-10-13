{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Control.Effect.IO
( rethrowing
) where

import Control.Effect.Error
import Control.Effect.Handler
import Control.Effect.Sum
import qualified Control.Exception as Exc
import Control.Monad.IO.Class

-- | Lift an 'IO' action into a carrier, catching and rethrowing any exceptions it throws into an 'Error' effect.
--
--   If you need more granular control over the types of exceptions caught, use 'catchIO' and rethrow in the handler.
rethrowing :: ( Member (Error Exc.SomeException) sig
              , MonadIO m
              , Carrier sig m
              )
           => IO a
           -> m a
rethrowing m = liftIO (Exc.try m) >>= either (throwError . Exc.toException @Exc.SomeException) pure
