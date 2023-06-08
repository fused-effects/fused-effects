{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Labelled
( example
) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative
#endif
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import qualified Control.Effect.Reader.Labelled as L
import qualified Control.Effect.State.Labelled as L
import           Hedgehog
import           Utils

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
  [ testProperty "runUnderLabel" . property $
    run (runReader (5 :: Int) (runLabelled @"fore" (runReader (10 :: Int) (runLabelled @"aft" sample)))) === 15
  , testProperty "Reader.Labelled helpers" . property $
    run (runReader (5 :: Int) (runLabelled @"fore" (runReader (10 :: Int) (runLabelled @"aft" withHelpers)))) === 15
  , testProperty "Nat labels" . property $
    run (runReader (5 :: Int) (runLabelled @1 (runReader (10 :: Int) (runLabelled @2 numerically)))) === 15
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
  [ testProperty "runUnderLabel" . property $
    run (evalState (5 :: Int) (runLabelled @"fore" (evalState (10 :: Int) (runLabelled @"aft" sampleS)))) === 15
  , testProperty "State.Labelled helpers" . property $
    run (evalState (5 :: Int) (runLabelled @"fore" (evalState (10 :: Int) (runLabelled @"aft" helpersS)))) === 15
  , testProperty "Boolean labels" . property $
    run (evalState (5 :: Int) (runLabelled @'True (evalState (10 :: Int) (runLabelled @'False boolean)))) === 15
  ]


example :: TestTree
example = testGroup "Control.Effect.Labelled"
  [ readerExamples
  , stateExamples
  ]
