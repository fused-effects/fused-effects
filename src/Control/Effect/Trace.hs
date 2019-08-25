{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Trace
( -- * Trace effect
  Trace(..)
, trace
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Carrier.Class
import GHC.Generics (Generic1)

data Trace m k = Trace
  { traceMessage :: String
  , traceCont    :: m k
  }
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Append a message to the trace log.
trace :: (Member Trace sig, Carrier sig m) => String -> m ()
trace message = send (Trace message (pure ()))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
