{-# LANGUAGE RankNTypes #-}
module Writer
( tests
) where

import qualified Control.Carrier.Writer.Strict as StrictWriterC
import Control.Effect.Writer
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriterT
import qualified Control.Monad.Trans.Writer.Strict as StrictWriterT
import Data.Tuple (swap)
import Gen.Writer
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Hedgehog.Range
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ testWriter "WriterC (Strict)" StrictWriterC.runWriter genW
  , testWriter "WriterT (Lazy)"   (fmap swap . LazyWriterT.runWriterT)   genW
  , testWriter "WriterT (Strict)" (fmap swap . StrictWriterT.runWriterT) genW
  ] where
    genW = list (linear 0 10) genA


testWriter :: (Has (Writer w) sig m, Arg w, Eq w, Monoid w, Show w, Vary w) => String -> (forall a . (m a -> PureC (w, a))) -> Gen w -> TestTree
testWriter name runWriter genW = testGroup name
  [ testProperty "tell append" . forall (genW :. fmap Blind (genM genWriter genW) :. Nil) $
    \ w m -> tell_append (~=) runWriter w (getBlind m)
  , testProperty "listen eavesdrop" . forall (fmap Blind (genM genWriter genW) :. Nil) $
    \ m -> listen_eavesdrop (~=) runWriter (getBlind m)
  , testProperty "censor revision" . forall (fn genW :. fmap Blind (genM genWriter genW) :. Nil) $
    \ f m -> censor_revision (~=) runWriter (apply f) (getBlind m)
  ]