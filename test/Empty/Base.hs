{-# LANGUAGE TypeApplications #-}
module Empty.Base
( tests
) where

import Control.Effect.Empty
import qualified Empty
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.Maybe"
  [ testProperty "empty annihilation" . forall (fn @A (Empty.gen genB) :. Nil) $
    \ k -> empty_annihilation (===) (id @(Maybe _)) (apply k)
  ]
