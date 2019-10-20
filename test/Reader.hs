{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
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
import Data.Functor.Identity (Identity(..))
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testGroup "ReaderC"       $
    [ testMonad
    , testReader
    ] >>= ($ RunR ReaderC.runReader)
  , testGroup "(->)"          $ testReader (RunR (fmap PureC . (&)))
  , testGroup "ReaderT"       $ testReader (RunR (flip ReaderT.runReaderT))
  , testGroup "RWST (Lazy)"   $ testReader (RunR (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)" $ testReader (RunR (runRWST StrictRWST.runRWST))
  ] where
  testMonad  (RunR run) = Monad.test    (m (gen r)) a b c ((,) <$> r <*> pure ()) (fmap Identity . uncurry run)
  testReader (RunR run) = Reader.test r (m (gen r)) a                                                      run
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r

newtype RunR r m = RunR (forall a . r -> m a -> PureC a)


gen
  :: forall r m a sig
  .  (Has (Reader r) sig m, Arg r, Show r, Vary r)
  => Gen r
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen r m a = choice
  [ label "asks" (asks @r) <*> fn a
  , label "local" local <*> fn r <*> m a
  ]


test
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r)
  => Gen r
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> (forall a . r -> m a -> PureC a)
  -> [TestTree]
test r m a runReader =
  [ testProperty "ask returns the environment variable" . forall (r :. fn (m a) :. Nil) $
    \ r k -> runReader r (ask >>= k) === runReader r (k r)
  , testProperty "local modifies the environment variable" . forall (r :. fn r :. m a :. Nil) $
    \ r f m -> runReader r (local f m) === runReader (f r) m
  ]
