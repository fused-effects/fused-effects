{- | An effect modelling catchable failure with a polymorphic error type, the combination of 'Throw' and 'Catch'.

This effect is similar to the traditional @MonadError@ typeclass, though it allows the presence of multiple @Error@ effects in a given effect stack. It offers precise exception handling, rather than the dynamic exception hierarchy provided by the @exceptions@ package. The @fused-effects-exceptions@ package may be more suitable for handling dynamic/impure effect handling.

Predefined carriers:

* "Control.Carrier.Error.Either".
* "Control.Monad.Trans.Except".
* If 'Error' @e@ is the last effect in a stack, it can be interpreted directly to an 'Either' @e@.

@since 0.1.0.0
-}

module Control.Effect.Error
( -- * Error effects
  Error
, module Control.Effect.Throw
, module Control.Effect.Catch
) where

import Control.Effect.Catch
import Control.Effect.Error.Internal (Error)
import Control.Effect.Throw
