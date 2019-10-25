{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Effect
( tests
) where

import Control.Algebra
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Monad.Trans.Class
import Data.Functor.Identity
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


-- | An effect to return the results of all of the nondeterministic branches computed within its scope.
data All m k = forall a . All (m a) ([a] -> m k)

deriving instance Functor m => Functor (All m)

-- | Note that since the continuation takes @[a]@, our 'Effect' instance will have to produce a continuation taking @[f a]@ given one taking @[a]@, for which purpose we use 'sequenceA'. That necessitates an 'Applicative' instance for @f@, so we specialize 'Constrain' for this effect to strengthen the constraint.
--
-- Intuitively, we can think of this as saying that if there are 'Control.Effect.State.State' effects present in @m@, collecting all of their results—joining the branches—requires us to merge the states monoidally; if there are 'Control.Effect.Error.Error' effects, it requires that they’ve all succeeded.
instance Effect All where
  type Constrain All f = Applicative f
  handle ctx hdl (All m k) = All (hdl (m <$ ctx)) (hdl . fmap k . sequenceA)

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


-- | A threading effect, much like in /Effect Handlers in Scope/. Note that the 'Fork' constructor takes an action producing @()@, which therefore can’t have any stateful handlers run inside it, but could still run e.g. 'Control.Carrier.Reader.ReaderC'.
data Thread m k
  = Fork (m ()) (m k)
  | Yield (m k)
  deriving (Functor)

-- | This demonstrates that we can use '~' to define 'Constrain', enforcing the absence of state or control that would get dropped.
--
-- We don’t go as far as to define a carrier because cooperative multithreading is a bit more of an example than a test, and I don’t care to run 'Control.Concurrent.forkIO' in the unit tests. At any rate, the existence of this instance is test enough for what we’re trying to do.
instance Effect Thread where
  type Constrain Thread f = f ~ Identity
  handle ctx hdl (Fork m k) = Fork (runIdentity <$> hdl (m <$ ctx)) (hdl (k <$ ctx))
  handle ctx hdl (Yield  k) = Yield                                 (hdl (k <$ ctx))
