{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Monad.Trans.Class
import Data.Monoid (Sum(..))
import Prelude hiding (all)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Effect"
  [ testCase "allows effects to constrain the state functor" $ do
    let bs = run . runAll . runState (sum 0) $ do
          s <- pure "hello" <|> pure "world"
          bs <- all (foldMapA (\ (c, i) -> c <$ put (sum i)) (zip s [1..]))
          bs <$ put (sum 1) <|> pure bs
    bs @?= [(Sum 1, "hello"), (Sum 15, "hello"), (Sum 1, "world"), (Sum 15, "world")]
  ] where
  sum i = Sum (i :: Int)


data All m k = forall a . All (m a) ([a] -> m k)

deriving instance Functor m => Functor (All m)

instance Effect All where
  type Constrain All f = Applicative f
  handle state handler (All m k) = All (handler (m <$ state)) (handler . fmap k . sequenceA)

all :: Has All sig m => m a -> m [a]
all m = send (All m pure)


runAll :: Applicative m => AllC m a -> m [a]
runAll (AllC m) = runNonDetA m

newtype AllC m a = AllC (NonDetC m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadTrans)

instance Algebra sig m => Algebra (All :+: NonDet :+: sig) (AllC m) where
  alg (L (All m k)) = lift (runAll m) >>= k
  alg (R (L (L e))) = handleCoercible e
  alg (R (L (R c))) = handleCoercible c
  alg (R (R other)) = handleCoercible other
