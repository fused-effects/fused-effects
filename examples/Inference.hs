{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances #-}
module Inference
( example
) where

import Control.Algebra
import Control.Carrier.Reader
import Test.Tasty
import Test.Tasty.QuickCheck

example :: TestTree
example = testGroup "inference"
  [ testProperty "type applications instantiate types" $
  -- Without @-XTypeApplications@ or some other constraint on the type, 'ask' would error: all @ghc@ would be able to prove about type of the 'Reader' effect, and thus the return type of 'ask', is that it’s a list of some kind. The type application allows us to specify it.
    \ x -> run (runEnv [x] ((++) <$> ask @String <*> ask @String))
    === [x, x]
  -- However, when the type is polymorphic, this can require contortions: @-XScopedTypeVariables@ and @forall@ annotations just to bring the type variables into scope, etc., and can be especially inconvenient in @ghci.
  --
  -- Sometimes we would like to be able to constrain the type by context instead. In these cases, we can use a @newtype@ with a phantom type parameter, plus a wrapper around 'ask' which uses that type parameter to constrain its return type, to provide enough context for the types to be inferred without annotation or @-XTypeApplications@.
  , testProperty "phantom type parameters constrain inference" $
    \ x -> run (runEnv [x] ((++) <$> askEnv <*> askEnv))
    === [x, x :: Integer]
  ]


-- | A constrained wrapper around 'ask'.
--
--   Like 'ask', 'askEnv' uses the same type parameter for both the 'Reader' and return types. Unlike 'ask'—which doesn’t impose any extra structure on the monad—it’s specialized to 'HasEnv', and uses the /same/ type parameter as its phantom type parameter.
--
--   Thus, any two calls to 'askEnv' occurring in the same 'HasEnv' context will be required to have their @env@ type parameters unify, allowing them to be inferred from context more often.
askEnv :: Has (Reader env) sig m => HasEnv env m env
askEnv = ask

-- | A handler for 'HasEnv' & 'ReaderC' with the same @env@ parameter.
--
--   Any 'askEnv's occurring in the second argument will have to unify not only with each other, but also with the first argument. Thus, if @ghc@ can infer the type of the any of these, it can infer all of them.
runEnv :: env -> HasEnv env (ReaderC env m) a -> m a
runEnv r = runReader r . runHasEnv


-- | The identity monad transformer, with an extra phantom type parameter.
newtype HasEnv env m a = HasEnv { runHasEnv :: m a }
  deriving (Applicative, Functor, Monad)

-- | The 'Carrier' instance for 'HasEnv' simply delegates all effects to the underlying carrier.
instance Algebra sig m => Algebra sig (HasEnv env m) where
  eff = HasEnv . eff . handleCoercible
