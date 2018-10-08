{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Carrier.Either
import Control.Effect

runFail :: Effect sig => Eff (Fail :+: sig) a -> Eff sig (Either String a)
runFail = runEitherH . relay alg
  where alg (Fail s) = EitherH (pure (Left s))
