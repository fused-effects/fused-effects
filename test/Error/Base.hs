{-# LANGUAGE TypeApplications #-}
module Error.Base
( tests
) where

import Control.Effect.Error
import qualified Error
import Hedgehog
import Hedgehog.Function hiding (C)
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.Either"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (Error.gen genC genB) :. Nil) $
    \ e k -> throwError_annihilation (===) (id @(Either C _)) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (Error.gen genC genA) :. Nil) $
    \ e f -> catchError_interception (===) (id @(Either C _)) e (apply f)
  ]
