{-# LANGUAGE TypeApplications #-}
module Empty.Maybe
( tests
) where

import Control.Carrier
import Control.Carrier.Empty.Maybe
import qualified Empty
import Hedgehog
import Hedgehog.Function
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty.Maybe.EmptyC"
  [ testProperty "empty annihilation" . forall (fn @A (Empty.gen genB) :. Nil) $
    \ k -> empty_annihilation (~=) runEmpty (apply k)
  ]

(~=) :: (Eq a, Show a) => PureC a -> PureC a -> PropertyT IO ()
m1 ~= m2 = run m1 === run m2
