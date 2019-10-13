{-# LANGUAGE RankNTypes #-}
module Reader
( tests
) where

import qualified Control.Carrier.Reader.Function as ReaderC
import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import Data.Function ((&))
import Gen.Reader
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testReader "ReaderC" ReaderC.runReader         genA
  , testReader "(->)"    (fmap PureC . (&))        genA
  , testReader "ReaderT" (flip ReaderT.runReaderT) genA
  , testReader "RWST (Lazy)"   (\ r m -> (\ (a, _, ()) -> a) <$> LazyRWST.runRWST   m r r) genA
  , testReader "RWST (Strict)" (\ r m -> (\ (a, _, ()) -> a) <$> StrictRWST.runRWST m r r) genA
  ]


testReader :: (Has (Reader r) sig m, Arg r, Eq r, Show r, Vary r) => String -> (forall a . r -> m a -> PureC a) -> Gen r -> TestTree
testReader name runReader genA = testGroup name
  [ testProperty "ask environment" . forall (genA :. fn (Blind <$> genM [genReader] genA) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn genA :. fmap Blind (genM [genReader] genA) :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]
