{-# LANGUAGE TypeApplications #-}
module Reader.ReaderT
( tests
, gen
) where

import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Hedgehog
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.ReaderT"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> gen genA) :. Nil) $
    \ a k -> ask_environment (~=) (flip Reader.runReaderT) a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (gen genA) :. Nil) $
    \ a f m -> local_modification (~=) (flip Reader.runReaderT) a (apply f) (getBlind m)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2


gen :: Carrier sig m => Gen a -> Gen (Reader.ReaderT a m a)
gen a = Gen.choice [ pure ask, pure <$> a ]
