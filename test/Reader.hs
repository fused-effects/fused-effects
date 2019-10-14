{-# LANGUAGE RankNTypes #-}
module Reader
( tests
, gen
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
  [ testGroup "ReaderC"       $ readerTests ReaderC.runReader
  , testGroup "(->)"          $ readerTests (fmap PureC . (&))
  , testGroup "ReaderT"       $ readerTests (flip ReaderT.runReaderT)
  , testGroup "RWST (Lazy)"   $ readerTests (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)" $ readerTests (runRWST StrictRWST.runRWST)
  ] where
  readerTests :: Has (Reader A) sig m => (forall a . A -> m a -> PureC a) -> [TestTree]
  readerTests run = Reader.readerTests run genA
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


gen :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
gen a ma = choice
  [ pure ask
  , fn a >>= subterm ma . local . apply
  ]


readerTests :: (Has (Reader r) sig m, Arg r, Eq r, Show r, Vary r) => (forall a . r -> m a -> PureC a) -> Gen r -> [TestTree]
readerTests runReader a =
  [ testProperty "ask environment" . forall (a :. fn (genM [gen] a) :. Nil) $
    \ a k -> ask_environment (~=) runReader a (getBlind . apply k)
  , testProperty "local modification" . forall (a :. fn a :. genM [gen] a :. Nil) $
    \ a f m -> local_modification (~=) runReader a (apply f) (getBlind m)
  ]
