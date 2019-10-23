{-# LANGUAGE TypeApplications #-}
module Cut.Church
( tests
) where

import Control.Carrier.Cut.Church
import Control.Effect.Reader
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Cut.Church"
  [ testCase "cutfail operates through higher-order effects" $
    (runCutA @[] (local (id @()) cutfail <|> pure 'a')) ()
    @?=
    (runCutA @[] (cutfail <|> pure 'a')) ()
  ]
