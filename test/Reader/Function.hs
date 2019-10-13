{-# LANGUAGE TypeApplications #-}
module Reader.Function
( tests
) where

import Control.Carrier.Reader.Function
import Hedgehog.Function
import Pure
import qualified Reader
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.Function.ReaderC"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> Reader.gen genA) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (Reader.gen genA) :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]
