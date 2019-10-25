{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving, TypeFamilies #-}
module Effect
( tests
) where

import Control.Algebra
import Test.Tasty

tests :: TestTree
tests = testGroup "Effect"
  []


data All m k = forall a . All (m a) ([a] -> m k)

deriving instance Functor m => Functor (All m)

instance Effect All where
  type Constrain All f = Applicative f
  handle state handler (All m k) = All (handler (m <$ state)) (handler . fmap k . sequenceA)
