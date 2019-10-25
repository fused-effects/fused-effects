{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving, TypeFamilies #-}
module Effect
( tests
) where

import Control.Effect.Class
import Test.Tasty

tests :: TestTree
tests = testGroup "Effect"
  []


data Try e m k
  = forall a . Try (m a) (Either e a -> m k)

deriving instance Functor m => Functor (Try e m)

instance Effect (Try e) where
  type Constrain (Try e) f = Applicative f
  handle state handler (Try m k) = Try (handler (m <$ state)) (handler . fmap k . sequenceA)
