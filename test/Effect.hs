{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Effect.Lift
import Control.Monad.IO.Class
import qualified Control.Exception as Exc
import Data.Monoid (Sum(..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Effect"
  [ testCase "allows monoidal state above effects requiring Applicative" $ do
    x <- liftIO . runTry @Exc.SomeException . execState (s 0) $ do
      put (s 1)
      v <- try @Exc.SomeException (put (s 2) *> liftIO (Exc.throwIO (Err "test")) *> put (s 3))
      either (pure . Exc.displayException) (pure . show) v
    x @?= s 0
  ] where
  s :: Int -> Sum Int
  s = Sum


newtype Err = Err { err :: String }
  deriving (Show)

instance Exc.Exception Err

data Try e m k
  = forall a . Try (m a) (Either e a -> m k)

deriving instance Functor m => Functor (Try e m)

instance Effect (Try e) where
  type Constrain (Try e) f = Applicative f
  handle state handler (Try m k) = Try (handler (m <$ state)) (handler . fmap k . sequenceA)

try :: Has (Try e) sig m => m a -> m (Either e a)
try m = send (Try m pure)


newtype TryC e m a = TryC { runTry :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Exc.Exception e => Algebra (Try e :+: Lift IO) (TryC e IO) where
  alg (L (Try m k)) = liftIO (Exc.try (runTry m)) >>= k
  alg (R other)     = TryC (handleCoercible other)
