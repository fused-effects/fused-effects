{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
, cut
, once
, Branch(..)
, branch
, runBranch
, runCut
, CutC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet
import Control.Effect.Sum

-- | 'Cut' effects are used with 'NonDet' to provide control over backtracking.
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)
  | forall a . Once (m a) (a -> k)

deriving instance Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) k
  hmap f (Once m k) = Once (f m) k
  {-# INLINE hmap #-}

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)
  handle state handler (Once m k) = Once (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
--   prop> run (runNonDet (runCut (cutfail <|> pure a))) == []
--   prop> run (runNonDet (runCut (pure a <|> cutfail))) == [a]
cutfail :: (Carrier sig m, Member Cut sig) => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
--   prop> run (runNonDet (runCut (call (cutfail <|> pure a) <|> pure b))) == [b]
call :: (Carrier sig m, Member Cut sig) => m a -> m a
call m = send (Call m ret)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDet (runCut (pure a <|> cut *> pure b))) == [a, b]
--   prop> run (runNonDet (runCut (cut *> pure a <|> pure b))) == [a]
--   prop> run (runNonDet (runCut (cut *> empty <|> pure a))) == []
cut :: (Alternative m, Carrier sig m, Member Cut sig) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}


-- | Prune nondeterminism in the argument, returning at most one result.
--
--   prop> run (runNonDet (runCut (once (pure a <|> pure b)))) == [a]
--   prop> run (runNonDet (runCut (once (pure a <|> pure b) <|> pure c))) == [a, c]
once :: (Carrier sig m, Member Cut sig) => m a -> m a
once m = send (Once m ret)
{-# INLINE once #-}


-- | The result of a nondeterministic branch of a computation.
data Branch m a
  = Cut
  | None
  | Pure a
  | Alt (m a) (m a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Case analysis for 'Branch', taking a value to use for 'Cut', a value to use for 'None', and a function to apply to the contents of 'Pure'.
--
--   prop> branch Cut None Pure Alt a == (a :: Branch [] a)
--   prop> branch a b (applyFun f) (applyFun2 g) (Cut :: Branch [] a) == a
--   prop> branch a b (applyFun f) (applyFun2 g) (None :: Branch [] a) == b
--   prop> branch a b (applyFun f) (applyFun2 g) (Pure c :: Branch [] a) == applyFun f c
--   prop> branch a b (applyFun f) (applyFun2 g) (Alt c d :: Branch [] a) == applyFun2 g c d
branch :: a -> a -> (b -> a) -> (m b -> m b -> a) -> Branch m b -> a
branch a _ _ _ Cut       = a
branch _ a _ _ None      = a
branch _ _ f _ (Pure a)  = f a
branch _ _ _ f (Alt a b) = f a b
{-# INLINE branch #-}

-- | Interpret a 'Branch' into an underlying 'Alternative' context.
runBranch :: Alternative m => Branch m a -> m a
runBranch = branch empty empty pure (<|>)
{-# INLINE runBranch #-}


-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically 'Eff' carrying a 'NonDet' effect).
--
--   prop> run (runNonDetOnce (runCut (pure a))) == Just a
runCut :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (CutC m) a -> m a
runCut = (>>= runBranch) . runCutC . interpret

newtype CutC m a = CutC { runCutC :: m (Branch m a) }

instance (Alternative m, Carrier sig m, Effect sig, Monad m) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  ret = CutC . ret . Pure
  {-# INLINE ret #-}

  eff = CutC . handleSum (handleSum
    (eff . handle (Pure ()) (bindBranch (ret Cut) runCutC))
    (\case
      Empty    -> ret None
      Choose k -> runCutC (k True) >>= branch (ret Cut) (runCutC (k False)) (\ a -> ret (Alt (ret a) (runCutC (k False) >>= runBranch))) (fmap ret . Alt)))
    (\case
      Cutfail  -> ret Cut
      Call m k -> runCutC m >>= bindBranch (ret None) (runCutC . k)
      Once m k -> runCutC m >>= bindBranch (ret Cut) (runCutC . k))
    where bindBranch :: (Alternative m, Carrier sig m, Monad m) => m (Branch m a) -> (b -> m (Branch m a)) -> Branch m b -> m (Branch m a)
          bindBranch cut bind = branch cut (ret None) bind (\ a b -> ret (Alt (a >>= bind >>= runBranch) (b >>= bind >>= runBranch)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> instance Arbitrary1 m => Arbitrary1 (Branch m) where liftArbitrary arbitrary = frequency [(1, pure Cut), (1, pure None), (3, Pure <$> arbitrary), (3, Alt <$> liftArbitrary arbitrary <*> liftArbitrary arbitrary)]
-- >>> instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Branch m a) where arbitrary = arbitrary1
