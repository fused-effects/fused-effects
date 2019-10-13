{-# LANGUAGE TypeApplications #-}
module Reader.Base
( tests
) where

import Control.Effect.Reader
import Data.Function ((&))
import Hedgehog
import Hedgehog.Function
import Pure
import qualified Reader
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.(->)"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> Reader.gen genA) :. Nil) $
    \ a k -> ask_environment (===) (&) a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (Reader.gen genA) :. Nil) $
    \ a f m -> local_modification (===) (&) a (apply f) (getBlind m)
  ]
