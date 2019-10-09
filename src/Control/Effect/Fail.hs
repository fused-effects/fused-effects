module Control.Effect.Fail
( -- * Fail effect
  Fail
, Fail.MonadFail(..)
  -- * Re-exports
, Has
) where

import Control.Effect.Throw
import qualified Control.Monad.Fail as Fail

type Fail = Throw String
