{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Resumable
( -- * Resumable effect
  Resumable(..)
, throwResumable
) where

import Control.Carrier

-- | Errors which can be resumed with values of some existentially-quantified type.
data Resumable err m k
  = forall a . Resumable (err a) (a -> m k)

deriving instance Functor m => Functor (Resumable err m)

instance HFunctor (Resumable err) where
  hmap f (Resumable err k) = Resumable err (f . k)

instance Effect (Resumable err) where
  handle state handler (Resumable err k) = Resumable err (handler . (<$ state) . k)

-- | Throw an error which can be resumed with a value of its result type.
--
--   prop> run (runResumable (throwResumable (Identity a))) === Left (SomeError (Identity a))
throwResumable :: (Member (Resumable err) sig, Carrier sig m) => err a -> m a
throwResumable err = send (Resumable err pure)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XGADTs
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
