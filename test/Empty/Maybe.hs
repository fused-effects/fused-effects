{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Empty.Maybe
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.Empty.Maybe
import Pure hiding (gen)
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.Maybe"
  [ testProperty "empty annihilation" . property $ forAllFn (fn @A (gen genB)) >>=
    \ k -> empty_annihilation (~=) k
  ]

(~=) :: (Eq a, Show a) => EmptyC PureC a -> EmptyC PureC a -> PropertyT IO ()
m1 ~= m2 = run (runEmpty m1) === run (runEmpty m2)


gen :: (Carrier sig m, Effect sig) => Gen a -> Gen (EmptyC m a)
gen a = Gen.choice [ pure empty, pure <$> a ]
