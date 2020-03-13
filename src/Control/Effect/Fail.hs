{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

{- | An effect providing failure with an error message.

This effect is invoked through the 'Control.Monad.Fail.fail' method from 'Control.Monad.Fail.MonadFail'.

Predefined carriers:

* "Control.Carrier.Fail.Either"

@since 0.1.0.0
-}

module Control.Effect.Fail
( -- * Fail effect
  Fail
, pattern Fail
, Fail.MonadFail(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Effect.Throw
import qualified Control.Monad.Fail as Fail

-- | @since 0.1.0.0
type Fail = Throw String

-- | @since 1.0.0.0
pattern Fail :: String -> Fail m k
pattern Fail s = Throw s

{-# COMPLETE Fail #-}
