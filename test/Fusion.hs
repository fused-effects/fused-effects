{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
module Fusion
( tests
) where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Gen
import Hedgehog
import Test.Inspection as Inspection hiding (property, (===))

tests :: TestTree
tests = testGroup "fusion"
  [ testProperty "eliminates StateCs" . property $
    failureOf $(inspectTest $ 'countDown `doesNotUse` ''StateC)
    === Nothing
  , testProperty "eliminates nested StateCs" . property $
    failureOf $(inspectTest $ 'countBoth `doesNotUse` ''StateC)
    === Nothing
  , testProperty "eliminates catch and throw" . property $
    failureOf $(inspectTest $ 'throwing `doesNotUse` ''ErrorC)
    === Nothing
  , testProperty "eliminates calls to alg" . property $
    failureOf $(inspectTest $ 'countDown `doesNotUse` 'alg)
    === Nothing
  ]


failureOf :: Inspection.Result -> Maybe String
failureOf (Success _) = Nothing
failureOf (Failure f) = Just f


countDown :: Int -> (Int, Int)
countDown start = run . runState start $ go
  where go = get >>= \n -> if n <= 0 then pure n else modify @Int pred *> go

countBoth :: Int -> (Int, (Float, ()))
countBoth n = run . runState n . runState (fromIntegral n) $ go where
  go = do
    n <- get @Int
    if
      | n == 0         -> pure ()
      | n `mod` 2 == 0 -> modify @Float (+ 1) *> modify @Int pred *> go
      | otherwise      -> modify @Int pred    *> go

throwing :: Int -> Either Int String
throwing n = run $ runError go
  where go = if n > 10 then throwError @Int 42 else pure "fine"
