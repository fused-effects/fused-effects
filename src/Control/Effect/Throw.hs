{- | An effect for polymorphic failure.

Predefined carriers:

* "Control.Carrier.Throw.Either"
* "Control.Carrier.Error.Either" (with 'Control.Effect.Catch.Catch')

@since 1.0.0.0
-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
, throwError
, liftEither
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Throw.Internal (Throw(..))

-- | Throw an error, escaping the current computation up to the nearest 'Control.Effect.Catch.catchError' (if any).
--
-- @
-- runThrow (throwError e >>= k) = runThrow (throwError e)
-- @
--
-- @since 0.1.0.0
throwError :: Has (Throw e) sig m => e -> m a
throwError = send . Throw
{-# INLINE throwError #-}

-- | Lifts an @'Either' e@ into Monad m with effect @'Throw' e@
--
-- @since 1.1.0.0
liftEither :: Has (Throw e) sig m => Either e a -> m a
liftEither = either throwError return
