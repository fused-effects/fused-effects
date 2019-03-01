{-# LANGUAGE DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, AltC(..)
, Branch(..)
, branch
, runBranch
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..), join)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce
import qualified Data.Monoid as Monoid (Alt(..))
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


-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: AltC f m a -> m (f a)
runNonDet = runAltC

newtype AltC f m a = AltC { runAltC :: m (f a) }
  deriving (Functor)

instance (Applicative f, Applicative m) => Applicative (AltC f m) where
  pure = AltC . pure . pure
  AltC f <*> AltC a = AltC (liftA2 (<*>) f a)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Alternative (AltC f m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Monad (AltC f m) where
  AltC a >>= f = AltC (a >>= runNonDet . Monoid.getAlt . foldMap (Monoid.Alt . f))

instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadFail m, Traversable f) => MonadFail (AltC f m) where
  fail s = AltC (fail s)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadIO m, Traversable f) => MonadIO (AltC f m) where
  liftIO io = AltC (pure <$> liftIO io)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => MonadPlus (AltC f m)

instance Applicative f => MonadTrans (AltC f) where
  lift m = AltC (pure <$> m)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Carrier (NonDet :+: sig) (AltC f m) where
  eff (L Empty)      = AltC (pure empty)
  eff (L (Choose k)) = AltC (liftA2 (<|>) (runNonDet (k True)) (runNonDet (k False)))
  eff (R other)      = AltC (eff (handle (pure ()) (fmap join . traverse runNonDet) other))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
-- >>> instance (Arbitrary1 m, Arbitrary e) => Arbitrary1 (Branch m e) where liftArbitrary arb = frequency [(1, None <$> arbitrary), (3, Pure <$> arb), (3, Alt <$> liftArbitrary arb <*> liftArbitrary arb)]
-- >>> instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (Branch m e a) where arbitrary = arbitrary1
