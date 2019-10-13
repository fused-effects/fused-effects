{-# LANGUAGE TypeApplications #-}
module Error.ExceptT
( tests
, gen
) where

import Control.Carrier
import Control.Effect.Error
import qualified Control.Monad.Trans.Except as Except
import Hedgehog
import Hedgehog.Function hiding (C)
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.ExceptT"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (gen genC genB) :. Nil) $
    \ e k -> throwError_annihilation (~=) Except.runExceptT e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (gen genC genA) :. Nil) $
    \ e f -> catchError_interception (~=) Except.runExceptT e (apply f)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: (Carrier sig m, Effect sig) => Gen e -> Gen a -> Gen (Except.ExceptT e m a)
gen e a = Gen.choice [ throwError <$> e, pure <$> a ]
