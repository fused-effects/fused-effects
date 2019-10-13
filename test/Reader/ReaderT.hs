{-# LANGUAGE TypeApplications #-}
module Reader.ReaderT
( tests
) where

import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Hedgehog.Function
import Pure
import qualified Reader
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader.ReaderT"
  [ testProperty "ask environment" . forall (genA :. fn @A (Blind <$> Reader.gen genA) :. Nil) $
    \ a k -> ask_environment (~=) (flip Reader.runReaderT) a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn @A genA :. fmap Blind (Reader.gen genA) :. Nil) $
    \ a f m -> local_modification (~=) (flip Reader.runReaderT) a (apply f) (getBlind m)
  ]
