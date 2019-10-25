{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.NonDet.Church
import Control.Monad.Trans.Class
import Test.Tasty

tests :: TestTree
tests = testGroup "Effect"
  []


data All m k = forall a . All (m a) ([a] -> m k)

deriving instance Functor m => Functor (All m)

instance Effect All where
  type Constrain All f = Applicative f
  handle state handler (All m k) = All (handler (m <$ state)) (handler . fmap k . sequenceA)


runAll :: Applicative m => AllC m a -> m [a]
runAll (AllC m) = runNonDetA m

newtype AllC m a = AllC (NonDetC m a)
  deriving (Alternative, Applicative, Functor, Monad, MonadTrans)

instance Algebra sig m => Algebra (All :+: NonDet :+: sig) (AllC m) where
  alg (L (All m k)) = lift (runAll m) >>= k
  alg (R (L (L e))) = handleCoercible e
  alg (R (L (R c))) = handleCoercible c
  alg (R (R other)) = handleCoercible other
