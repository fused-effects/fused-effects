{-# LANGUAGE DeriveTraversable, KindSignatures #-}
module Control.Effect.NonDet.Internal
( NonDet(..)
, Branch(..)
, branch
, runBranch
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Data.Coerce

data NonDet (m :: * -> *) k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance HFunctor NonDet where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect NonDet where
  handle _     _       Empty      = Empty
  handle state handler (Choose k) = Choose (handler . (<$ state) . k)


-- | The result of a nondeterministic branch of a computation.
--
--   'Branch' can be used to define 'NonDet' carriers which control nondeterminism in some specific way, e.g. pruning branches according to some specific heuristic.
data Branch m e a
  = None e
  | Pure a
  | Alt (m a) (m a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Alternative m => Applicative (Branch m e) where
  pure = Pure
  None e    <*> _         = None e
  Pure f    <*> a         = f <$> a
  Alt _  _  <*> None e    = None e
  Alt f1 f2 <*> Pure a    = Alt (f1 <*> pure a) (f2 <*> pure a)
  Alt f1 f2 <*> Alt a1 a2 = Alt (f1 <*> a1 <|> f1 <*> a2) (f2 <*> a1 <|> f2 <*> a2)


-- | Case analysis for 'Branch', taking a value to use for 'Cut', a value to use for 'None', and a function to apply to the contents of 'Pure'.
--
--   prop> branch None Pure Alt a == (a :: Branch e [] a)
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (None a :: Branch [] a) == applyFun f a
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (Pure a :: Branch [] a) == applyFun g a
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (Alt a b :: Branch [] a) == applyFun2 h a b
branch :: (e -> a) -> (b -> a) -> (m b -> m b -> a) -> Branch m e b -> a
branch f _ _ (None a)  = f a
branch _ f _ (Pure a)  = f a
branch _ _ f (Alt a b) = f a b
{-# INLINE branch #-}

-- | Interpret a 'Branch' into an underlying 'Alternative' context.
runBranch :: Alternative m => (e -> m a) -> Branch m e a -> m a
runBranch f = branch f pure (<|>)
{-# INLINE runBranch #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
-- >>> instance (Arbitrary1 m, Arbitrary e) => Arbitrary1 (Branch m e) where liftArbitrary arb = frequency [(1, None <$> arbitrary), (3, Pure <$> arb), (3, Alt <$> liftArbitrary arb <*> liftArbitrary arb)]
-- >>> instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (Branch m e a) where arbitrary = arbitrary1
