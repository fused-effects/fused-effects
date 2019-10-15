{-# LANGUAGE RankNTypes #-}
module Cull
( tests
, gen
, test
) where

import qualified Control.Carrier.Cull.Church as CullC
import Control.Effect.Choose
import Control.Effect.Cull
import Control.Effect.NonDet (NonDet)
import Gen
import qualified NonDet
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cull"
  [ testGroup "CullC" $ test (m gen) a b CullC.runCullA
  ]


gen
  :: (Has Cull sig m, Has NonDet sig m, Show a)
  => (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = choice
  [ addLabel "cull" (atom "cull" cull <*> m a)
  , NonDet.gen m a
  ]


test
  :: (Has Cull sig m, Has NonDet sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC [a])
  -> [TestTree]
test m a b runCull
  = testProperty "cull returns at most one success" (forall (a :. m a :. m a :. Nil)
    (\ a m n -> runCull (cull (pure a <|> m) <|> n) === runCull (pure a <|> n)))
  : NonDet.test m a b runCull
