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
  -> (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen r m a = choice
  [ addLabel "ask" (atom "asks" (asks @r) <*> fn a)
  , addLabel "local" (atom "local" local <*> fn r <*> m a)
  ]


test
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r)
  => Gen r
  -> (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> (forall a . r -> m a -> PureC a)
  -> [TestTree]
test r m a runReader =
  [ testProperty "ask returns the environment variable" . forall (r :. fn (m a) :. Nil) $
    \ r k -> runReader r (ask >>= k) === runReader r (k r)
  , testProperty "local modifies the environment variable" . forall (r :. fn r :. m a :. Nil) $
    \ r f m -> runReader r (local f m) === runReader (f r) m
  ]
