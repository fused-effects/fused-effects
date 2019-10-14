{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
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
import Hedgehog.Function hiding (C, R)
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
  readerTests :: Has (Reader (T R)) sig m => (forall a . T R -> m a -> PureC a) -> [TestTree]
  readerTests run = Reader.readerTests run (genM (gen r)) r a
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


gen :: forall r m a sig . (Has (Reader r) sig m, Arg r, Vary r) => Gen r -> (forall a . Gen a -> Gen (m a)) -> Gen a -> Gen (m a)
gen r m a = choice
  [ asks @r . apply <$> fn a
  , fn r >>= subterm (m a) . local . apply
  ]


readerTests :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r) => (forall a . r -> m a -> PureC a) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen r -> Gen a -> [TestTree]
readerTests runReader m r a =
  [ testProperty "ask environment" . forall (r :. fn (m a) :. Nil) $
    \ r k -> ask_environment (~=) runReader r (getBlind . apply k)
  , testProperty "local modification" . forall (r :. fn r :. m a :. Nil) $
    \ r f m -> local_modification (~=) runReader r (apply f) (getBlind m)
  ]
