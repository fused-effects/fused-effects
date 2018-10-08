{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, runNonDetSplit
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Carrier.List
import Control.Carrier.Split
import Control.Effect

runNonDet :: Effect sig => Eff (NonDet :+: sig) a -> Eff sig [a]
runNonDet = runListH . relay alg
  where alg Empty      = ListH (pure [])
        alg (Choose k) = ListH (liftA2 (++) (runListH (k True)) (runListH (k False)))

runNonDetSplit :: Effect sig => Eff (NonDet :+: sig) a -> Eff sig [a]
runNonDetSplit = joinSplitH . relay alg
  where alg Empty      = empty
        alg (Choose k) = SplitH (runSplitH (k True) >>= maybe (runSplitH (k False)) (\ (a, q) -> pure (Just (a, q <|> k False))))
