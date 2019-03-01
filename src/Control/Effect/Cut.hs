{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
, cut
, runCut
, CutC(..)
, ListC(..)
, runListAll
, runListAlt
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

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
runCut :: Alternative m => CutC m a -> m a
runCut m = runCutC m ((<|>) . pure) empty empty

newtype CutC m a = CutC { runCutC :: forall b . (a -> m b -> m b) -> m b -> m b -> m b }
  deriving (Functor)

runCutAll :: (Alternative f, Applicative m) => CutC m a -> m (f a)
runCutAll (CutC m) = m (fmap . (<|>) . pure) (pure empty) (pure empty)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail

instance Alternative (CutC m) where
  empty = CutC (\ _ nil _ -> nil)
  CutC l <|> CutC r = CutC (\ cons nil fail -> l cons (r cons nil fail) fail)

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCutC (f a') cons as fail) nil fail

instance MonadFail m => MonadFail (CutC m) where
  fail s = CutC (\ _ _ _ -> fail s)

instance MonadIO m => MonadIO (CutC m) where
  liftIO io = CutC (\ cons nil _ -> liftIO io >>= flip cons nil)

instance MonadPlus (CutC m)

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)

instance (Carrier sig m, Effect sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff (L Cutfail)    = CutC $ \ _    _   fail -> fail
  eff (L (Call m k)) = CutC $ \ cons nil fail -> runCutC m (\ a as -> runCutC (k a) cons as fail) nil nil
  eff (R (L Empty))      = empty
  eff (R (L (Choose k))) = k True <|> k False
  eff (R (R other)) = CutC $ \ cons nil _ -> eff (handle [()] (fmap concat . traverse runCutAll) other) >>= foldr cons nil
  {-# INLINE eff #-}


newtype ListC m a = ListC { runListC :: forall b . (a -> m b -> m b) -> m b -> m b }
  deriving (Functor)

runListAll :: (Alternative f, Applicative m) => ListC m a -> m (f a)
runListAll (ListC m) = m (fmap . (<|>) . pure) (pure empty)

runListAlt :: Alternative m => ListC m a -> m a
runListAlt (ListC m) = m ((<|>) . pure) empty

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

instance MonadTrans ListC where
  lift m = ListC (\ cons nil -> m >>= flip cons nil)

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (ListC m) where
  eff (L Empty) = empty
  eff (L (Choose k)) = k True <|> k False
  eff (R other) = ListC $ \ cons nil -> eff (handle [()] (fmap concat . traverse runListAll) other) >>= foldr cons nil


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Cull
-- >>> import Control.Effect.Void
