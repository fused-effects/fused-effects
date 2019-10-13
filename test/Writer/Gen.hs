{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer.Gen
( genWriter
, writerTests
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

genWriter :: forall a m sig . (Has (Writer a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
genWriter a ma = choice
  [ tell' <$> a
  , subtermM ma (\ m -> element [fst <$> listen @a m, snd <$> listen @a m])
  , fn a >>= subterm ma . censor . apply
  ] where
  tell' a = a <$ tell a

writerTests :: (Has (Writer w) sig m, Arg w, Eq w, Monoid w, Show w, Vary w) => (forall a . (m a -> PureC (w, a))) -> Gen w -> [TestTree]
writerTests runWriter genW =
  [ testProperty "tell append" . forall (genW :. fmap Blind (genM genWriter genW) :. Nil) $
    \ w m -> tell_append (~=) runWriter w (getBlind m)
  , testProperty "listen eavesdrop" . forall (fmap Blind (genM genWriter genW) :. Nil) $
    \ m -> listen_eavesdrop (~=) runWriter (getBlind m)
  , testProperty "censor revision" . forall (fn genW :. fmap Blind (genM genWriter genW) :. Nil) $
    \ f m -> censor_revision (~=) runWriter (apply f) (getBlind m)
  ]
