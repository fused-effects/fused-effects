{-# LANGUAGE TypeApplications #-}
module Empty.MaybeT
( tests
) where

import Control.Effect.Empty
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Empty
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.MaybeT"
  [ testProperty "empty annihilation" . forall (fn @A (Empty.gen genB) :. Nil) $
    \ k -> empty_annihilation (~=) Maybe.runMaybeT (apply k)
  ]
