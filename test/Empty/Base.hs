{-# LANGUAGE TypeApplications #-}
module Empty.Base
( tests
, gen
) where

import Control.Effect.Empty
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.Maybe"
  [ testProperty "empty annihilation" . forall (fn @A (gen genB) :. Nil) $
    \ k -> empty_annihilation (===) (apply k)
  ]


gen :: Gen a -> Gen (Maybe a)
gen a = Gen.choice [ pure empty, pure <$> a ]
