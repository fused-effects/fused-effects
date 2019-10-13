{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module Writer
( gen
, genWriter
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Gen

gen :: Has (Writer a) sig m => Gen a -> Gen (m a)
gen a = choice [ genWriter a (gen a), pure <$> a ]

genWriter :: forall a m sig . Has (Writer a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genWriter a ma = choice
  [ tell' <$> a
  , subtermM ma (\ m -> element [fst <$> listen @a m, snd <$> listen @a m])
  ] where
  tell' a = a <$ tell a
