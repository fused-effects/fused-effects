{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeApplications, TypeFamilies #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Carrier.State.Strict
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
  , testCase "allows arbitrary state above effects requiring Functor" $ do
    x <- liftIO . runCatchIO . execState (s 0) $ do
      put (s 1)
      (put (s 2) *> liftIO (Exc.throwIO (Err "test")) *> put (s 3)) `catchIO` \ (Err _) -> pure ()
    x @?= s 1
  ] where
  s :: Int -> Sum Int
  s = Sum


data All m k = forall a . All (m a) ([a] -> m k)

deriving instance Functor m => Functor (All m)

instance Effect All where
  type Constrain All f = Applicative f
  handle state handler (All m k) = All (handler (m <$ state)) (handler . fmap k . sequenceA)


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

instance Algebra Try (TryC IO) where
  alg (Try m k) = liftIO (Exc.try (runTry m)) >>= k


data CatchIO m k
  = forall e a . Exc.Exception e => CatchIO (m a) (e -> m a) (a -> m k)

deriving instance Functor m => Functor (CatchIO m)

instance Effect CatchIO where
  handle state handler (CatchIO m h k) = CatchIO (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

catchIO :: (Exc.Exception e, Has CatchIO sig m) => m a -> (e -> m a) -> m a
catchIO m h = send (CatchIO m h pure)


newtype CatchIOC m a = CatchIOC { runCatchIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra CatchIO (CatchIOC IO) where
  alg (CatchIO m h k) = liftIO (runCatchIO m `Exc.catch` (runCatchIO . h)) >>= k
