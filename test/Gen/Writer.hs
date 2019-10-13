{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module Gen.Writer
( gen
, genWriter
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

gen :: (Has (Writer a) sig m, Arg a, Vary a) => Gen a -> Gen (m a)
gen a = choice [ genWriter a (gen a), pure <$> a ]

genWriter :: forall a m sig . (Has (Writer a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
genWriter a ma = choice
  [ tell' <$> a
  , subtermM ma (\ m -> element [fst <$> listen @a m, snd <$> listen @a m])
  , fn a >>= subterm ma . censor . apply
  ] where
  tell' a = a <$ tell a
