{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
, cut
, runCut
, CutC(..)
, BacktrackC(..)
, runBacktrackAll
, runBacktrackAlt
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.State
import Control.Effect.Sum

-- | 'Cut' effects are used with 'NonDet' to provide control over backtracking.
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)

deriving instance Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) k
  {-# INLINE hmap #-}

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)
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
call m = send (Call m pure)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDet (runCut (pure a <|> cut *> pure b))) == [a, b]
--   prop> run (runNonDet (runCut (cut *> pure a <|> pure b))) == [a]
--   prop> run (runNonDet (runCut (cut *> empty <|> pure a))) == []
cut :: (Alternative m, Carrier sig m, Member Cut sig) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}


-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically another 'Carrier' for a 'NonDet' effect).
--
--   prop> run (runNonDetOnce (runCut (pure a))) == Just a
runCut :: (Alternative m, Carrier sig m) => CutC m a -> m a
runCut = evalState True . runBacktrackAlt . runCutC

newtype CutC m a = CutC { runCutC :: BacktrackC (StateC Bool m) a }
  deriving (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Alternative (CutC m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance (Carrier sig m, Effect sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff = CutC . handleSum (handleSum
    (eff . R . R . handleCoercible)
    (\case
      Empty    -> BacktrackC $ \ _    nil -> nil
      Choose k -> BacktrackC $ \ cons nil -> do
        let handle a as = do
              shouldBacktrack <- get
              if shouldBacktrack then do
                maybe id cons a $ runBacktrackC (runCutC (k False)) cons as
              else
                maybe nil (flip cons nil) a
        runBacktrackC (runCutC (k True)) (handle . Just) (handle Nothing nil)))
    (\case
      Cutfail  -> BacktrackC $ \ _    nil -> put False *> nil
      Call m k -> BacktrackC $ \ cons nil -> do
        shouldBacktrack <- get
        runBacktrackC (runCutC m) (\ a as -> put shouldBacktrack *> runBacktrackC (runCutC (k a)) cons as) (put (shouldBacktrack :: Bool) *> nil))
  {-# INLINE eff #-}


newtype BacktrackC m a = BacktrackC { runBacktrackC :: forall b . (a -> m b -> m b) -> m b -> m b }
  deriving (Functor)

runBacktrackAll :: (Alternative f, Applicative m) => BacktrackC m a -> m (f a)
runBacktrackAll (BacktrackC m) = m (fmap . (<|>) . pure) (pure empty)

runBacktrackAlt :: Alternative m => BacktrackC m a -> m a
runBacktrackAlt (BacktrackC m) = m ((<|>) . pure) empty

instance Applicative (BacktrackC m) where
  pure a = BacktrackC (\ cons nil -> cons a nil)
  BacktrackC f <*> BacktrackC a = BacktrackC $ \ cons nil ->
    f (\ f' -> a (cons . f')) nil

instance Alternative (BacktrackC m) where
  empty = BacktrackC (\ _ nil -> nil)
  BacktrackC l <|> BacktrackC r = BacktrackC $ \ cons nil ->
    l cons (r cons nil)

instance Monad (BacktrackC m) where
  BacktrackC a >>= f = BacktrackC $ \ cons nil ->
    a (\ a' -> runBacktrackC (f a') cons) nil

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (BacktrackC m) where
  eff (L Empty) = empty
  eff (L (Choose k)) = k True <|> k False
  eff (R other) = BacktrackC $ \ cons nil -> eff (handle [()] (fmap concat . traverse runBacktrackAll) other) >>= foldr cons nil


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


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
