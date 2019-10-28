{- | An effect modelling nondeterminism without choice (success or failure).

This can be seen as similar to 'Control.Effect.Fail.Fail', but without an error message. The 'Control.Effect.NonDet.NonDet' effect is the composition of 'Empty' and 'Control.Effect.Choice.Choice'.

Predefined carriers:

* "Control.Carrier.Empty.Maybe".
* If 'Empty' is the last effect in a stack, it can be interpreted directly to a 'Maybe'.

@since 1.0.0.0
-}

module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
, guard
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Empty.Internal (Empty(..))

-- | Abort the computation.
--
-- 'empty' annihilates '>>=':
--
-- @
-- 'empty' '>>=' k = 'empty'
-- @
--
-- @since 1.0.0.0
empty :: Has Empty sig m => m a
empty = send Empty

-- | Conditional failure, returning only if the condition is 'True'.
--
-- @since 1.0.0.0
guard :: Has Empty sig m => Bool -> m ()
guard True  = pure ()
guard False = empty
