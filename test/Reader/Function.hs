{-# LANGUAGE TypeApplications #-}
module Reader.Function
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.Reader.Function
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.Function"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (gen genA) :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: Carrier sig m => Gen a -> Gen (ReaderC a m a)
gen a = Gen.choice [ pure ask, pure <$> a ]
