{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer
( tests
, gen
, writerTests
) where

import qualified Control.Carrier.Writer.Strict as StrictWriterC
import Control.Effect.Writer
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriterT
import qualified Control.Monad.Trans.Writer.Strict as StrictWriterT
import Data.Tuple (swap)
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ testGroup "WriterC (Strict)" $ writerTests StrictWriterC.runWriter
  , testGroup "WriterT (Lazy)"   $ writerTests (fmap swap . LazyWriterT.runWriterT)
  , testGroup "WriterT (Strict)" $ writerTests (fmap swap . StrictWriterT.runWriterT)
  , testGroup "RWST (Lazy)"      $ writerTests (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)"    $ writerTests (runRWST StrictRWST.runRWST)
  ] where
  writerTests :: Has (Writer W) sig m => (forall a . m a -> PureC (W, a)) -> [TestTree]
  writerTests run = Writer.writerTests run (genM (gen w)) w a
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen :: forall w m a sig . (Has (Writer w) sig m, Arg w, Show a, Show w, Vary w) => Gen w -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen w m a = choice
  [ (<*>) . (atom "(<$)" (<$) <*>) . showing <$> a <*> ((atom "tell" tell <*>) . showing <$> w)
  , subtermM (m a) (\ m -> choice
    [(\ f -> (liftWith2 "fmap" fmap (liftWith2 "(.)" (.) (showingFn f) (atom "fst" fst)) (atom "listen" (listen @w) <*> m))) <$> fn a
    , pure (liftWith2 "fmap" fmap (atom "snd" snd) (atom "listen" (listen @w) <*> m))
    ])
  , fn w >>= subterm (m a) . liftWith2 "censor" censor . showingFn
  ]


writerTests :: (Has (Writer w) sig m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w) => (forall a . (m a -> PureC (w, a))) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen w -> Gen a -> [TestTree]
writerTests runWriter m w a =
  [ testProperty "tell append" . forall (w :. m a :. Nil) $
    \ w m -> tell_append (===) runWriter w (getWith m)
  , testProperty "listen eavesdrop" . forall (m a :. Nil) $
    \ m -> listen_eavesdrop (===) runWriter (getWith m)
  , testProperty "censor revision" . forall (fn w :. m a :. Nil) $
    \ f m -> censor_revision (===) runWriter (apply f) (getWith m)
  ]
