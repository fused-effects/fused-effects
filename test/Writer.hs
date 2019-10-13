{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer
( tests
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
import Hedgehog.Range
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
  writerTests :: Has (Writer [A]) sig m => (forall a . m a -> PureC ([A], a)) -> [TestTree]
  writerTests run = Writer.writerTests run genW
  genW = list (linear 0 10) genA
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


genWriter :: forall a m sig . (Has (Writer a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
genWriter a ma = choice
  [ tell' <$> a
  , subtermM ma (\ m -> element [fst <$> listen @a m, snd <$> listen @a m])
  , fn a >>= subterm ma . censor . apply
  ] where
  tell' a = a <$ tell a


writerTests :: (Has (Writer w) sig m, Arg w, Eq w, Monoid w, Show w, Vary w) => (forall a . (m a -> PureC (w, a))) -> Gen w -> [TestTree]
writerTests runWriter genW =
  [ testProperty "tell append" . forall (genW :. fmap Blind (genM [genWriter] genW) :. Nil) $
    \ w m -> tell_append (~=) runWriter w (getBlind m)
  , testProperty "listen eavesdrop" . forall (fmap Blind (genM [genWriter] genW) :. Nil) $
    \ m -> listen_eavesdrop (~=) runWriter (getBlind m)
  , testProperty "censor revision" . forall (fn genW :. fmap Blind (genM [genWriter] genW) :. Nil) $
    \ f m -> censor_revision (~=) runWriter (apply f) (getBlind m)
  ]
