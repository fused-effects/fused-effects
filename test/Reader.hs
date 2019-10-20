{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
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
import GHC.Generics ((:.:)(..))
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Reader"
  [ testGroup "ReaderC" $
    [ testMonad
    , testMonadFix
    , testReader
    ] >>= ($ runR (uncurry ReaderC.runReader . lower))
  , testGroup "(->)"          $ testReader (runR (uncurry (fmap PureC . (&))           . lower))
  , testGroup "ReaderT"       $ testReader (runR (uncurry (flip ReaderT.runReaderT)    . lower))
  , testGroup "RWST (Lazy)"   $ testReader (runR (uncurry (runRWST LazyRWST.runRWST)   . lower))
  , testGroup "RWST (Strict)" $ testReader (runR (uncurry (runRWST StrictRWST.runRWST) . lower))
  ] where
  testMonad    run = Monad.test    (m (gen r)) a b c (Comp1 <$> (identity <*> (atom "(,)" (,) <*> r <*> unit))) run
  testMonadFix run = MonadFix.test (m (gen r)) a b   (Comp1 <$> (identity <*> (atom "(,)" (,) <*> r <*> unit))) run
  testReader   run = Reader.test r (m (gen r)) a                (identity <*>                           unit)   run
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r
  lower = runIdentity . unComp1


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
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r, Functor f)
  => Gen r
  -> GenM m
  -> Gen a
  -> Gen (f ())
  -> Run (f :.: (,) r) Identity m
  -> [TestTree]
test r m a i (Run runReader) =
  [ testProperty "ask returns the environment variable" . forall (i :. r :. fn (m a) :. Nil) $
    \ i r k -> runReader (Comp1 ((r, ask >>= k) <$ i)) === runReader (Comp1 ((r, k r) <$ i))
  , testProperty "local modifies the environment variable" . forall (i :. r :. fn r :. m a :. Nil) $
    \ i r f m -> runReader (Comp1 ((r, local f m) <$ i)) === runReader (Comp1 ((f r, m) <$ i))
  ]
