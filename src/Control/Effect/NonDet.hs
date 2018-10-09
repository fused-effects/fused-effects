{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, runNonDetOnce
, runNonDetSplit
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect

runNonDet :: TermMonad m sig => Eff (ListH m) a -> m [a]
runNonDet = runListH . runEff var

newtype ListH m a = ListH { runListH :: m [a] }

instance Carrier [] ListH where
  joinl mf = ListH (mf >>= runListH)

  suspend f = f [()]

  resume = fmap concat . traverse runListH

  wrap = ListH

instance TermMonad m sig => TermAlgebra (ListH m) (NonDet :+: sig) where
  var a = ListH (pure [a])
  con = alg \/ algRest
    where alg Empty = ListH (pure [])
          alg (Choose k) = ListH (liftA2 (++) (runListH (k True)) (runListH (k False)))

runNonDetOnce :: TermMonad m sig => Eff (MaybeH m) a -> m (Maybe a)
runNonDetOnce = runMaybeH . runEff var

newtype MaybeH m a = MaybeH { runMaybeH :: m (Maybe a) }

instance Carrier Maybe MaybeH where
  joinl mf = MaybeH (mf >>= runMaybeH)

  suspend f = f (Just ())

  resume = maybe (pure Nothing) runMaybeH

  wrap = MaybeH

instance TermMonad m sig => TermAlgebra (MaybeH m) (NonDet :+: sig) where
  var a = MaybeH (pure (Just a))
  con = alg \/ algRest
    where alg Empty      = MaybeH (pure Nothing)
          alg (Choose k) = MaybeH (liftA2 (<|>) (runMaybeH (k True)) (runMaybeH (k False)))

runNonDetSplit :: TermMonad m sig => Eff (SplitH m) a -> m [a]
runNonDetSplit = joinSplitH . runEff var

newtype SplitH m a = SplitH { runSplitH :: m (Maybe (a, SplitH m a)) }

joinSplitH :: Monad m => SplitH m a -> m [a]
joinSplitH = (>>= maybe (pure []) (\ (a, q) -> (a :) <$> joinSplitH q)) . runSplitH

instance Monad m => Semigroup (SplitH m a) where
  a <> b = SplitH (runSplitH a >>= maybe (runSplitH b) (\ (a', q) -> pure (Just (a', q <> b))))

instance Monad m => Monoid (SplitH m a) where
  mempty = SplitH (pure Nothing)

instance Carrier [] SplitH where
  joinl a = SplitH (a >>= runSplitH)

  suspend f = f [()]

  resume []     = pure []
  resume (a:as) = runSplitH a >>= maybe (resume as) (\ (a', q) -> (a' :) <$> resume (q : as))

  wrap a = SplitH (a >>= \ a' -> case a' of
    []     -> pure Nothing
    a'':as -> pure (Just (a'', wrap (pure as))))

instance TermMonad m sig => TermAlgebra (SplitH m) (NonDet :+: sig) where
  var a = SplitH (pure (Just (a, SplitH (pure Nothing))))
  con = alg \/ algRest
    where alg Empty      = SplitH (pure Nothing)
          alg (Choose k) = SplitH (runSplitH (k True) >>= maybe (runSplitH (k False)) (\ (a, q) -> pure (Just (a, q <> k False))))
