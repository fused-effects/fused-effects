{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, runNonDetOnce
, runNonDetSplit
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum

runNonDet :: TermMonad sig m => Eff (ListH m) a -> m [a]
runNonDet = runListH . interpret

newtype ListH m a = ListH { runListH :: m [a] }

instance TermMonad sig m => Carrier (NonDet :+: sig) (ListH m) where
  gen a = ListH (pure [a])
  con = alg \/ (ListH . con . handle [()] (fmap concat . traverse runListH))
    where alg Empty = ListH (pure [])
          alg (Choose k) = ListH (liftA2 (++) (runListH (k True)) (runListH (k False)))


runNonDetOnce :: TermMonad sig m => Eff (MaybeH m) a -> m (Maybe a)
runNonDetOnce = runMaybeH . interpret

newtype MaybeH m a = MaybeH { runMaybeH :: m (Maybe a) }

instance TermMonad sig m => Carrier (NonDet :+: sig) (MaybeH m) where
  gen a = MaybeH (pure (Just a))
  con = alg \/ (MaybeH . con . handle (Just ()) (maybe (pure Nothing) runMaybeH))
    where alg Empty      = MaybeH (pure Nothing)
          alg (Choose k) = MaybeH (liftA2 (<|>) (runMaybeH (k True)) (runMaybeH (k False)))


runNonDetSplit :: TermMonad sig m => Eff (SplitH m) a -> m [a]
runNonDetSplit = joinSplitH . interpret

newtype SplitH m a = SplitH { runSplitH :: m (Maybe (a, SplitH m a)) }

joinSplitH :: Monad m => SplitH m a -> m [a]
joinSplitH = (>>= maybe (pure []) (\ (a, q) -> (a :) <$> joinSplitH q)) . runSplitH

instance Monad m => Semigroup (SplitH m a) where
  a <> b = SplitH (runSplitH a >>= maybe (runSplitH b) (\ (a', q) -> pure (Just (a', q <> b))))

instance Monad m => Monoid (SplitH m a) where
  mempty = SplitH (pure Nothing)

instance TermMonad sig m => Carrier (NonDet :+: sig) (SplitH m) where
  gen a = SplitH (pure (Just (a, SplitH (pure Nothing))))
  con = alg \/ (wrap . con . handle [()] (fmap concat . traverse joinSplitH))
    where alg Empty      = SplitH (pure Nothing)
          alg (Choose k) = SplitH (runSplitH (k True) >>= maybe (runSplitH (k False)) (\ (a, q) -> pure (Just (a, q <> k False))))

          wrap a = SplitH (a >>= \ a' -> case a' of
            []     -> pure Nothing
            a'':as -> pure (Just (a'', wrap (pure as))))
