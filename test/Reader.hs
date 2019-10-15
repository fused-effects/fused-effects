{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Reader
( tests
, gen
, test
) where

import qualified Control.Carrier.Reader as ReaderC
import Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import Data.Function ((&))
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testGroup "ReaderC"       $ test r (m (gen r)) a ReaderC.runReader
  , testGroup "(->)"          $ test r (m (gen r)) a (fmap PureC . (&))
  , testGroup "ReaderT"       $ test r (m (gen r)) a (flip ReaderT.runReaderT)
  , testGroup "RWST (Lazy)"   $ test r (m (gen r)) a (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)" $ test r (m (gen r)) a (runRWST StrictRWST.runRWST)
  ] where
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


gen
  :: forall r m a sig
  .  (Has (Reader r) sig m, Arg r, Show a, Show r, Vary r)
  => Gen r
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen (With (m a))
gen r m a = choice
  [ liftWith "asks" (asks @r) . showingFn <$> fn a
  , fn r >>= subterm (m a) . liftWith2 "local" local . showingFn
  ]


test
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r)
  => Gen r
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> (forall a . r -> m a -> PureC a)
  -> [TestTree]
test r m a runReader =
  [ testProperty "ask returns the environment variable" . forall (r :. fn (m a) :. Nil) $
    \ r (FnWith k) -> runReader r (ask >>= k) === runReader r (k r)
  , testProperty "local modifies the environment variable" . forall (r :. fn r :. m a :. Nil) $
    \ r (Fn f) (With m) -> runReader r (local f m) === runReader (f r) m
  ]
