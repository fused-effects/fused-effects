{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, TypeOperators, TypeApplications, UndecidableInstances #-}
module Error.Either
( tests
, gen
) where

import Control.Carrier
import Control.Carrier.Error.Either
import Hedgehog
import Hedgehog.Function hiding (C)
import qualified Hedgehog.Gen as Gen
import Pure hiding (gen)
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.Either"
  [ testProperty "throwError annihilation" . property . forall (genC ::: fn @A (gen genC genB) ::: Nil) $
    \ e k -> throwError_annihilation (~=) e (apply k)
  , testProperty "catchError interception" . property . forall (genC ::: fn @C (gen genC genA) ::: Nil) $
    \ e f -> catchError_interception (~=) e (apply f)
  ]

(~=) :: (Eq e, Eq a, Show e, Show a) => ErrorC e PureC a -> ErrorC e PureC a -> PropertyT IO ()
m1 ~= m2 = run (runError m1) === run (runError m2)


gen :: (Carrier sig m, Effect sig) => Gen e -> Gen a -> Gen (ErrorC e m a)
gen e a = Gen.choice [ throwError <$> e, pure <$> a ]


infixr 5 :::

data Rec as where
  Nil :: Rec '[]
  (:::) :: a -> Rec as -> Rec (a ': as)

class Forall g f | g -> f, f -> g where
  forall :: g -> f -> PropertyT IO ()

instance Forall (Rec '[]) (PropertyT IO ()) where
  forall Nil = id

instance (Forall (Rec gs) b, Show a) => Forall (Rec (Gen a ': gs)) (a -> b) where
  forall (g ::: gs) f = do
    a <- forAll g
    forall gs (f a)
