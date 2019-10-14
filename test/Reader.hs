{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Reader
( tests
, gen
, readerTests
) where

import qualified Control.Carrier.Reader.Function as ReaderC
import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import Data.Function ((&))
import Hedgehog
import Hedgehog.Function hiding (R)
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
  readerTests :: Has (Reader R) sig m => (forall a . R -> m a -> PureC a) -> [TestTree]
  readerTests run = Reader.readerTests run (genM (gen r)) r a
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


gen :: forall r m a sig . (Has (Reader r) sig m, Arg r, Show a, Show r, Vary r) => Gen r -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen r m a = choice
  [ liftWith "asks" (asks @r) . showingFn <$> fn a
  , fn r >>= subterm (m a) . liftWith2 "local" local . showingFn
  ]


readerTests :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r) => (forall a . r -> m a -> PureC a) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen r -> Gen a -> [TestTree]
readerTests runReader m r a =
  [ testProperty "ask returns the environment variable" . forall (r :. fn (m a) :. Nil) $
    \ r (FnWith k) -> runReader r (ask >>= k) === runReader r (k r)
  , testProperty "local modifies the environment variable" . forall (r :. fn r :. m a :. Nil) $
    \ r (Fn f) (With m) -> runReader r (local f m) === runReader (f r) m
  ]
