{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( -- * Cut effect
  Cut(..)
, cutfail
, call
, cut
  -- * Cut carrier
, runCut
, runCutAll
, CutC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | 'Cut' effects are used with 'NonDet' to provide control over backtracking.
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> m k)

deriving instance Functor m => Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
--   prop> run (runNonDet (runCut (cutfail <|> pure a))) === []
--   prop> run (runNonDet (runCut (pure a <|> cutfail))) === [a]
cutfail :: (Carrier sig m, Member Cut sig) => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
--   prop> run (runNonDet (runCut (call (cutfail <|> pure a) <|> pure b))) === [b]
call :: (Carrier sig m, Member Cut sig) => m a -> m a
call m = send (Call m pure)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDet (runCut (pure a <|> cut *> pure b))) === [a, b]
--   prop> run (runNonDet (runCut (cut *> pure a <|> pure b))) === [a]
--   prop> run (runNonDet (runCut (cut *> empty <|> pure a))) === []
cut :: (Alternative m, Carrier sig m, Member Cut sig) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}


-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically another 'Carrier' for a 'NonDet' effect).
--
--   prop> run (runNonDetOnce (runCut (pure a))) === Just a
runCut :: Alternative m => CutC m a -> m a
runCut m = runCutC m ((<|>) . pure) empty empty

-- | Run a 'Cut' effect, returning all its results in an 'Alternative' collection.
runCutAll :: (Alternative f, Applicative m) => CutC m a -> m (f a)
runCutAll (CutC m) = m (fmap . (<|>) . pure) (pure empty) (pure empty)

newtype CutC m a = CutC
  { -- | A higher-order function receiving three parameters: a function to combine each solution with the rest of the solutions, an action to run when no results are produced (e.g. on 'empty'), and an action to run when no results are produced and backtrcking should not be attempted (e.g. on 'cutfail').
    runCutC :: forall b . (a -> m b -> m b) -> m b -> m b -> m b
  }
  deriving (Functor)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Alternative (CutC m) where
  empty = CutC (\ _ nil _ -> nil)
  {-# INLINE empty #-}
  CutC l <|> CutC r = CutC (\ cons nil fail -> l cons (r cons nil fail) fail)
  {-# INLINE (<|>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCutC (f a') cons as fail) nil fail
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (CutC m) where
  fail s = CutC (\ _ _ _ -> fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC (\ cons nil _ -> mfix (\ a -> runCutC (f (head a)) (fmap . (:)) (pure []) (pure [])) >>= foldr cons nil)
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (CutC m) where
  liftIO io = CutC (\ cons nil _ -> liftIO io >>= flip cons nil)
  {-# INLINE liftIO #-}

instance MonadPlus (CutC m)

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff (L Cutfail)    = CutC $ \ _    _   fail -> fail
  eff (L (Call m k)) = CutC $ \ cons nil fail -> runCutC m (\ a as -> runCutC (k a) cons as fail) nil nil
  eff (R (L Empty))      = empty
  eff (R (L (Choose k))) = k True <|> k False
  eff (R (R other)) = CutC $ \ cons nil _ -> eff (handle [()] (fmap concat . traverse runCutAll) other) >>= foldr cons nil
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Cull
-- >>> import Control.Effect.Pure
