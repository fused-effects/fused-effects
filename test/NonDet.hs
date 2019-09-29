{-# LANGUAGE FlexibleContexts, QuantifiedConstraints, RankNTypes, TypeApplications, TypeOperators #-}
module NonDet
( tests
) where

import Control.Carrier
import Control.Carrier.Error.Either
import qualified Control.Carrier.NonDet.Church as Church
import Control.Carrier.State.Strict
import Control.Effect.NonDet
import Prelude hiding (error)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "NonDet"
  [ interactions "Church" Church.runNonDet
  ]

interactions
  :: ( forall sig m . (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (t m)
     , forall m . Alternative (t m)
     )
  => String
  -> (forall m x . Applicative m => t m x -> m [x])
  -> TestTree
interactions title runNonDet = testGroup title
  [ testCase "collects results of effects run inside it" $
    run (runNonDet (runState 'a' state))
    @?= [('a', 'z'), ('b', 'b'), ('a', 'a')]
  , testCase "collapses results of effects run outside it" $
    run (runState 'a' (runNonDet (state @(NonDet :+: State Char :+: Pure))))
    @?= ('b', "zbb")
  , testCase "collects results from higher-order effects run inside it" $
    run (runNonDet (runError (error @(Error Char :+: NonDet :+: Pure))))
    @?= [Right 'z', Right 'a' :: Either Char Char]
  , testCase "collapses results of higher-order effects run outside it" $
    run (runError (runNonDet (error @(NonDet :+: Error Char :+: Pure))))
    @?= (Right "a" :: Either Char String)
  ]

state :: (Has (State Char) sig m, Alternative m) => m Char
state = pure 'z' <|> put 'b' *> get <|> get

error :: (Has (Error Char) sig m, Alternative m) => m Char
error = (pure 'z' <|> throwError 'a') `catchError` pure
