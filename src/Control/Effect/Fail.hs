{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Carrier.Either
import Control.Effect

runFail :: TermMonad m sig => Eff (Fail :+: sig) a -> m (Either String a)
runFail = runEitherH . interpret alg
  where alg (Fail s) = EitherH (pure (Left s))
