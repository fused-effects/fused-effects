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
import Data.Functor.Identity (Identity)
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testGroup "ReaderC"       $
    [ testMonad
    , testMonadFix
    , testReader
    ] >>= ($ RunR (uncurry ReaderC.runReader))
  , testGroup "(->)"          $ testReader (RunR (uncurry (fmap PureC . (&))))
  , testGroup "ReaderT"       $ testReader (RunR (uncurry (flip ReaderT.runReaderT)))
  , testGroup "RWST (Lazy)"   $ testReader (RunR (uncurry (runRWST LazyRWST.runRWST)))
  , testGroup "RWST (Strict)" $ testReader (RunR (uncurry (runRWST StrictRWST.runRWST)))
  ] where
  testMonad    run = Monad.test    (m (gen r)) a b c (atom "(,)" (,) <*> r <*> unit) run
  testMonadFix run = MonadFix.test (m (gen r)) a b   (atom "(,)" (,) <*> r <*> unit) run
  testReader   run = Reader.test r (m (gen r)) a                                     run
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r


gen
  :: forall r m sig
  .  (Has (Reader r) sig m, Arg r, Show r, Vary r)
  => Gen r
  -> GenM m
  -> GenM m
gen r m a = choice
  [ label "asks" (asks @r) <*> fn a
  , label "local" local <*> fn r <*> m a
  ]


test
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r)
  => Gen r
  -> GenM m
  -> Gen a
  -> RunC r Identity m
  -> [TestTree]
test r m a (RunC runReader) =
  [ testProperty "ask returns the environment variable" . forall (r :. fn (m a) :. Nil) $
    \ r k -> runReader r (ask >>= k) === runReader r (k r)
  , testProperty "local modifies the environment variable" . forall (r :. fn r :. m a :. Nil) $
    \ r f m -> runReader r (local f m) === runReader (f r) m
  ]
