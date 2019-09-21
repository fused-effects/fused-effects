{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
) where

import Control.Carrier
import GHC.Generics (Generic1)

-- | An effect modelling nondeterminism without choice.
--
--   This can be seen as similar to 'Control.Effect.Fail.Fail', but without an error message.
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty

-- | Abort the computation.
--
--   prop> run (runEmpty empty) === Nothing
empty :: (Carrier sig m, Member Empty sig) => m a
empty = send Empty


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Empty.Maybe
