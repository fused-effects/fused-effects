{-# LANGUAGE MultiWayIf, TemplateHaskell, TypeApplications #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
module Fusion
( tests
) where

import Control.Carrier.Class
import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Test.Inspection as Inspection
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "fusion"
  [ testCase "eliminates StateCs" $
    failureOf $(inspectTest $ 'countDown `doesNotUse` ''StateC)
    @?= Nothing
  , testCase "eliminates nested StateCs" $
    failureOf $(inspectTest $ 'countBoth `doesNotUse` ''StateC)
    @?= Nothing
  , testCase "eliminates catch and throw" $
    failureOf $(inspectTest $ 'throwing `doesNotUse` ''ErrorC)
    @?= Nothing
  , testCase "eliminates calls to eff" $
    failureOf $(inspectTest $ 'countDown `doesNotUse` 'eff)
    @?= Nothing
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
