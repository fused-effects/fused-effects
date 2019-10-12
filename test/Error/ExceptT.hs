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
    \ e k -> throwError_annihilation (~=) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (gen genC genA) :. Nil) $
    \ e f -> catchError_interception (~=) e (apply f)
  ]

(~=) :: (Eq e, Eq a, Show e, Show a) => Except.ExceptT e PureC a -> Except.ExceptT e PureC a -> PropertyT IO ()
m1 ~= m2 = run (Except.runExceptT m1) === run (Except.runExceptT m2)


gen :: (Carrier sig m, Effect sig) => Gen e -> Gen a -> Gen (Except.ExceptT e m a)
gen e a = Gen.choice [ throwError <$> e, pure <$> a ]
