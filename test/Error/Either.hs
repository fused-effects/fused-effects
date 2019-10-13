{-# LANGUAGE TypeApplications #-}
module Error.Either
( tests
) where

import Control.Carrier
import Control.Carrier.Error.Either
import qualified Error
import Hedgehog
import Hedgehog.Function hiding (C)
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.Either.ErrorC"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (Error.gen genC genB) :. Nil) $
    \ e k -> throwError_annihilation (~=) (runError @C) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (Error.gen genC genA) :. Nil) $
    \ e f -> catchError_interception (~=) (runError @C) e (apply f)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2
