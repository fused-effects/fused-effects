{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Empty.Maybe
( tests
) where

import Control.Carrier.Empty.Maybe
import Control.Monad.Trans.Maybe
import Pure
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Empty.Maybe"
  [ testProperty "empty annihilation" $
    \ k -> empty_annihilation ((~=) @B) (applyFun @A k)
  ]

(~=) :: (Eq a, Show a) => EmptyC PureC a -> EmptyC PureC a -> Property
m1 ~= m2 = run (runEmpty m1) === run (runEmpty m2)


instance Arbitrary1 m => Arbitrary1 (EmptyC m) where
  liftArbitrary genA = EmptyC . MaybeT <$> liftArbitrary @m (liftArbitrary @Maybe genA)
  liftShrink shrinkA = map (EmptyC . MaybeT) . liftShrink (liftShrink shrinkA) . runEmpty

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (EmptyC m a) where
  arbitrary = arbitrary1
  shrink = shrink1
