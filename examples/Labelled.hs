{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Labelled
( example
) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Applicative
import Control.Effect.Labelled
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import qualified Control.Effect.Reader.Labelled as L
import qualified Control.Effect.State.Labelled as L

sample :: ( HasLabelled "fore" (Reader Int) sig m
          , HasLabelled "aft" (Reader Int) sig m
          )
       => m Int
sample = liftA2 (+) (runUnderLabel @"fore" ask) (runUnderLabel @"aft" ask)

withHelpers :: ( HasLabelled "fore" (Reader Int) sig m
               , HasLabelled "aft" (Reader Int) sig m
               )
            => m Int
withHelpers = liftA2 (+) (L.ask @"fore") (L.ask @"aft")

numerically :: ( HasLabelled 1 (Reader Int) sig m
               , HasLabelled 2 (Reader Int) sig m
               )
            => m Int
numerically = liftA2 (+) (L.ask @1) (L.ask @2)

readerExamples :: TestTree
readerExamples = testGroup "Reader"
  [ testCase "runUnderLabel"           (run (runReader (5 :: Int) (runLabelled @"fore" (runReader (10 :: Int) (runLabelled @"aft" sample)))) @=? 15)
  , testCase "Reader.Labelled helpers" (run (runReader (5 :: Int) (runLabelled @"fore" (runReader (10 :: Int) (runLabelled @"aft" withHelpers)))) @=? 15)
  , testCase "Nat labels"              (run (runReader (5 :: Int) (runLabelled @1 (runReader (10 :: Int) (runLabelled @2 numerically)))) @=? 15)
  ]

sampleS :: ( HasLabelled "fore" (State Int) sig m
          , HasLabelled "aft" (State Int) sig m
          )
       => m Int
sampleS = liftA2 (+) (runUnderLabel @"fore" get) (runUnderLabel @"aft" get)

helpersS :: ( HasLabelled "fore" (State Int) sig m
               , HasLabelled "aft" (State Int) sig m
               )
            => m Int
helpersS = liftA2 (+) (L.get @"fore") (L.get @"aft")

boolean :: ( HasLabelled 'True (State Int) sig m
           , HasLabelled 'False (State Int) sig m
           )
            => m Int
boolean = liftA2 (+) (L.get @'True) (L.get @'False)

stateExamples :: TestTree
stateExamples = testGroup "State"
  [ testCase "runUnderLabel"          (run (evalState (5 :: Int) (runLabelled @"fore" (evalState (10 :: Int) (runLabelled @"aft" sampleS)))) @=? 15)
  , testCase "State.Labelled helpers" (run (evalState (5 :: Int) (runLabelled @"fore" (evalState (10 :: Int) (runLabelled @"aft" helpersS)))) @=? 15)
  , testCase "Boolean labels"         (run (evalState (5 :: Int) (runLabelled @'True (evalState (10 :: Int) (runLabelled @'False boolean)))) @=? 15)
  ]


example :: TestTree
example = testGroup "Control.Effect.Labelled"
  [ readerExamples
  , stateExamples
  ]

