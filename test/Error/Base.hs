{-# LANGUAGE TypeApplications #-}
module Error.Base
( tests
, gen
) where

import Control.Effect.Error
import Hedgehog
import Hedgehog.Function hiding (C)
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.Either"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (gen genC genB) :. Nil) $
    \ e k -> throwError_annihilation (===) id e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (gen genC genA) :. Nil) $
    \ e f -> catchError_interception (===) id e (apply f)
  ]


gen :: Gen e -> Gen a -> Gen (Either e a)
gen e a = Gen.choice [ throwError <$> e, pure <$> a ]
