{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect.NonDet where

import Control.Applicative (liftA2)
import Control.Carrier.List
import Control.Effect

runNonDet :: Effect sig => Eff (NonDet :+: sig) a -> Eff sig [a]
runNonDet = runListH . relay alg
  where alg Empty      = ListH (pure [])
        alg (Choose k) = ListH (liftA2 (++) (runListH (k True)) (runListH (k False)))
