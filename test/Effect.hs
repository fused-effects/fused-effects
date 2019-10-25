{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Effect.Lift
import Control.Monad.IO.Class
import qualified Control.Exception as Exc
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


newtype TryC e m a = TryC { runTry :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Exc.Exception e => Algebra (Try e :+: Lift IO) (TryC e IO) where
  alg (L (Try m k)) = liftIO (Exc.try (runTry m)) >>= k
  alg (R other)     = TryC (handleCoercible other)
