{-# LANGUAGE TypeApplications #-}
module Error.Base
( tests
) where

import Control.Effect.Error
import Error
import Hedgehog
import Hedgehog.Function hiding (C)
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.Either"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (genM [genError genC] genB) :. Nil) $
    \ e k -> throwError_annihilation (===) (id @(Either C _)) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (genM [genError genC] genA) :. Nil) $
    \ e f -> catchError_interception (===) (id @(Either C _)) e (apply f)
  ]
