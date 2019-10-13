{-# LANGUAGE RankNTypes #-}
module Reader
( genReader
, tests
) where

import qualified Control.Carrier.Reader.Function as ReaderC
import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import Data.Function ((&))
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testReader "ReaderC" ReaderC.runReader
  , testReader "(->)"    (fmap PureC . (&))
  , testReader "ReaderT" (flip ReaderT.runReaderT)
  , testReader "RWST (Lazy)"   (runRWST LazyRWST.runRWST)
  , testReader "RWST (Strict)" (runRWST StrictRWST.runRWST)
  ] where
  testReader :: Has (Reader A) sig m => String -> (forall a . A -> m a -> PureC a) -> TestTree
  testReader name run = Reader.testReader name run genA
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


genReader :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
genReader a ma = choice
  [ pure ask
  , fn a >>= subterm ma . local . apply
  ]


testReader :: (Has (Reader r) sig m, Arg r, Eq r, Show r, Vary r) => String -> (forall a . r -> m a -> PureC a) -> Gen r -> TestTree
testReader name runReader genA = testGroup name
  [ testProperty "ask environment" . forall (genA :. fn (Blind <$> genM [genReader] genA) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (genA :. fn genA :. fmap Blind (genM [genReader] genA) :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]
