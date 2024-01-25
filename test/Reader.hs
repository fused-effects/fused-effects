{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Reader
( tests
, gen0
, genN
, test
) where

import qualified Control.Carrier.Reader as ReaderC
import           Control.Effect.Reader
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import           Data.Function ((&))
import           Gen
import           GHC.Generics ((:.:)(..))
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "Reader"
  [ testGroup "ReaderC" $
    [ testMonad
    , testMonadFix
    , testReader
    ] >>= ($ runR (uncurry ReaderC.runReader . lower))
  , testGroup "(->)"          $ testReader (runR (uncurry (fmap pure . (&))            . lower))
  , testGroup "ReaderT"       $ testReader (runR (uncurry (flip ReaderT.runReaderT)    . lower))
  , testGroup "RWST (Lazy)"   $ testReader (runR (uncurry (runRWST LazyRWST.runRWST)   . lower))
  , testGroup "RWST (Strict)" $ testReader (runR (uncurry (runRWST StrictRWST.runRWST) . lower))
  ] where
  testMonad    run = Monad.test        (genM (gen0 termR) (genN termR)) termA termB termC (Comp1 <$> (identity <*> (pair <*> termR <*> unit))) run
  testMonadFix run = MonadFix.test     (genM (gen0 termR) (genN termR)) termA termB       (Comp1 <$> (identity <*> (pair <*> termR <*> unit))) run
  testReader   run = Reader.test termR (genM (gen0 termR) (genN termR)) termA                        (identity <*>                     unit)   run
  runRWST f r m = (\ (a, _, ()) -> a) <$> f m r r
  lower = runIdentity . unComp1


gen0
  :: forall r m a sig
  .  (Has (Reader r) sig m, Arg r, Show r, Vary r)
  => GenTerm r
  -> GenTerm a
  -> [GenTerm (m a)]
gen0 _ a = [ label "asks" (asks @r) <*> fn a ]

genN
  :: (Has (Reader r) sig m, Arg r, Show r, Vary r)
  => GenTerm r
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN r m a = [ subtermM (m a) (label "local" local <*> fn r <*>) ]


test
  :: (Has (Reader r) sig m, Arg r, Eq a, Show a, Show r, Vary r, Functor f)
  => GenTerm r
  -> GenM m
  -> GenTerm a
  -> GenTerm (f ())
  -> Run (f :.: (,) r) Identity m
  -> [TestTree]
test r m a i (Run runReader) =
  [ testProperty "ask returns the environment variable" . forall_ (i :. r :. fn (m a) :. Nil) $
    \ i r k -> runReader (Comp1 ((r, ask >>= k) <$ i)) === runReader (Comp1 ((r, k r) <$ i))
  , testProperty "local modifies the environment variable" . forall_ (i :. r :. fn r :. m a :. Nil) $
    \ i r f m -> runReader (Comp1 ((r, local f m) <$ i)) === runReader (Comp1 ((f r, m) <$ i))
  ]
