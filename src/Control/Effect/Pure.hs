{- | Provides a mechanism to kick off the evaluation of an effect stack in a pure computation.

This is generally the last effect in an effect stack, unless that stack needs to delegate to a base monad with the 'Control.Effect.Lift.Lift' effect. Such stacks are invoked with 'Control.Carrier.Pure.run' once all of their constituent effects have been discharged.

Predefined carriers:

* "Control.Carrier.Pure"
* 'Data.Functor.Identity.Identity'

@since 0.3.0.0
-}

module Control.Effect.Pure
( -- * Pure effect
  Pure
) where

import Control.Algebra.Internal
