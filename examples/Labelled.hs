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

sample :: ( HasLabelled "fore" (Reader Int) sig m
          , HasLabelled "aft" (Reader Int) sig m
          )
       => m Int
sample = liftA2 (+) (runUnderLabel @"fore" ask) (runUnderLabel @"aft" ask)

example :: TestTree
example = testGroup "labelled"
  [ testCase "labelled reader" ((run (runReader (5 :: Int) (runLabelled @"fore" (runReader (10 :: Int) (runLabelled @"aft" sample))))) @=? 15)
  ]
