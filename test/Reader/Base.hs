{-# LANGUAGE TypeApplications #-}
module Reader.Base
( tests
, gen
) where

import Control.Effect.Reader
import Data.Function ((&))
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.(->)"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> ask_environment (===) (&) a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (gen genA) :. Nil) $
    \ a f m -> local_modification (===) (&) a (apply f) (getBlind m)
  ]


gen :: Gen a -> Gen (a -> a)
gen a = Gen.choice [ pure ask, pure <$> a ]
