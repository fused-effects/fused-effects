{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Error.Either
() where

import Control.Carrier.Error.Either
import Control.Monad.Trans.Except
import Test.QuickCheck

instance (Arbitrary e, Arbitrary1 m) => Arbitrary1 (ErrorC e m) where
  liftArbitrary genA = ErrorC . ExceptT <$> liftArbitrary @m (liftArbitrary2 @Either (arbitrary @e) genA)
  liftShrink shrinkA = map (ErrorC . ExceptT) . liftShrink (liftShrink2 shrink shrinkA) . runError
