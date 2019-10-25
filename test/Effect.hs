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
    x <- liftIO . runTry . execState (s 0) $ do
      put (s 1)
      v <- try (put (s 2) *> liftIO (Exc.throwIO (Err "test")) *> put (s 3))
      either (pure . Exc.displayException @Err) (pure . show) v
    x @?= s 0
  ] where
  s :: Int -> Sum Int
  s = Sum


newtype Err = Err { err :: String }
  deriving (Show)

instance Exc.Exception Err

data Try m k
  = forall e a . Exc.Exception e => Try (m a) (Either e a -> m k)

deriving instance Functor m => Functor (Try m)

instance Effect Try where
  type Constrain Try f = Applicative f
  handle state handler (Try m k) = Try (handler (m <$ state)) (handler . fmap k . sequenceA)

try :: (Exc.Exception e, Has Try sig m) => m a -> m (Either e a)
try m = send (Try m pure)


newtype TryC m a = TryC { runTry :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra (Try :+: Lift IO) (TryC IO) where
  alg (L (Try m k)) = liftIO (Exc.try (runTry m)) >>= k
  alg (R other)     = TryC (handleCoercible other)
