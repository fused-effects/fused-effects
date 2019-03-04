{-# LANGUAGE DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, runListAll
, runListAlt
, ListC(..)
, Branch(..)
, branch
, runBranch
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Coerce
import Prelude hiding (fail)

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


data BTree a = Nil | Leaf a | Branch (BTree a) (BTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BTree where
  pure = Leaf
  Nil          <*> _ = Nil
  Leaf f       <*> a = fmap f a
  Branch f1 f2 <*> a = Branch (f1 <*> a) (f2 <*> a)

instance Alternative BTree where
  empty = Nil
  (<|>) = Branch

instance Monad BTree where
  Nil          >>= _ = Nil
  Leaf a       >>= f = f a
  Branch a1 a2 >>= f = Branch (a1 >>= f) (a2 >>= f)


-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: (Alternative f, Applicative m) => ListC m a -> m (f a)
runNonDet = runListAll


runListAll :: (Alternative f, Applicative m) => ListC m a -> m (f a)
runListAll (ListC m) = m (fmap . (<|>) . pure) (pure empty)

runListAlt :: Alternative m => ListC m a -> m a
runListAlt (ListC m) = m ((<|>) . pure) empty

newtype ListC m a = ListC { runListC :: forall b . (a -> m b -> m b) -> m b -> m b }
  deriving (Functor)

instance Applicative (ListC m) where
  pure a = ListC (\ cons -> cons a)
  ListC f <*> ListC a = ListC $ \ cons ->
    f (\ f' -> a (cons . f'))

instance Alternative (ListC m) where
  empty = ListC (\ _ nil -> nil)
  ListC l <|> ListC r = ListC $ \ cons -> l cons . r cons

instance Monad (ListC m) where
  ListC a >>= f = ListC $ \ cons ->
    a (\ a' -> runListC (f a') cons)

instance MonadFail m => MonadFail (ListC m) where
  fail s = ListC (\ _ _ -> fail s)

instance MonadIO m => MonadIO (ListC m) where
  liftIO io = ListC (\ cons nil -> liftIO io >>= flip cons nil)

instance MonadPlus (ListC m)

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (ListC m) where
  eff (L Empty) = empty
  eff (L (Choose k)) = k True <|> k False
  eff (R other) = ListC $ \ cons nil -> eff (handle [()] (fmap concat . traverse runListAll) other) >>= foldr cons nil


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
-- >>> instance (Arbitrary1 m, Arbitrary e) => Arbitrary1 (Branch m e) where liftArbitrary arb = frequency [(1, None <$> arbitrary), (3, Pure <$> arb), (3, Alt <$> liftArbitrary arb <*> liftArbitrary arb)]
-- >>> instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (Branch m e a) where arbitrary = arbitrary1
