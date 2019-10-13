{-# LANGUAGE TypeApplications #-}
module Empty.Maybe
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.Empty.Maybe
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.Maybe.EmptyC"
  [ testProperty "empty annihilation" . forall (fn @A (gen genB) :. Nil) $
    \ k -> empty_annihilation (~=) runEmpty (apply k)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: (Carrier sig m, Effect sig) => Gen a -> Gen (EmptyC m a)
gen a = Gen.choice [ pure empty, pure <$> a ]
