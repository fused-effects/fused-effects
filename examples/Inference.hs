{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances #-}
module Inference
( example
) where

import Control.Carrier
import Control.Carrier.Reader
import Test.Tasty
import Test.Tasty.HUnit

example :: TestTree
example = testGroup "inference"
  [ testCase "allows unwrapped accessors" $
  -- Without @-XTypeApplications@ or some other constraint on the type, 'ask' would error: all @ghc@ would be able to prove about type of the 'Reader' effect, and thus the return type of 'ask', is that it’s a list of some kind. The type application allows us to specify it.
    run (runHasEnv (runEnv "i" ((++) <$> ask @String <*> ask @String)))
    @?= "ii"
  -- However, when the type is polymorphic, this can require contortions: @-XScopedTypeVariables@ and @forall@ annotations just to bring the type variables into scope, etc., and can be especially inconvenient in @ghci.
  --
  -- Sometimes we would like to be able to constrain the type by context instead. In these cases, we can use a @newtype@ with a phantom type parameter, plus a wrapper around 'ask' which uses that type parameter to constrain its return type, to provide enough context for the types to be inferred without annotation or @-XTypeApplications@.
  , testCase "can be wrapped for better type inference" $
    run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv)))
    @?= "ii"
  ]


askEnv :: Has (Reader env) sig m => HasEnv env m env
askEnv = ask

runEnv :: env -> HasEnv env (ReaderC env m) a -> HasEnv env m a
runEnv r = HasEnv . runReader r . runHasEnv


newtype HasEnv env m a = HasEnv { runHasEnv :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig m => Carrier sig (HasEnv env m) where
  eff = HasEnv . eff . handleCoercible
