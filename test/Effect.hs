{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Effect
( tests
) where

import Test.Tasty

tests :: TestTree
tests = testGroup "Effect"
  []


data Try e m k
  = forall a . Try (m a) (Either e a -> m k)

deriving instance Functor m => Functor (Try e m)
