{-# LANGUAGE FlexibleContexts #-}
module Writer
( gen
, genWriter
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Gen

gen :: Has (Writer a) sig m => Gen a -> Gen (m a)
gen a = choice [ genWriter a, pure <$> a ]

genWriter :: Has (Writer a) sig m => Gen a -> Gen (m a)
genWriter a = choice
  [ tell' <$> a
  ] where
  tell' a = a <$ tell a
