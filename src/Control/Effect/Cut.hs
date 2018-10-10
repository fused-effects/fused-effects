{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut where

import Control.Applicative (Alternative(..))
import Control.Effect

data Cut m k
  = Cut
  | forall b . Call (m b) (b -> k)

deriving instance Functor (Cut m)

instance Effect Cut where
  hfmap _ Cut        = Cut
  hfmap f (Call m k) = Call (f m) k

  handle _     _       Cut        = Cut
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)

cutfail :: (Subset Cut sig, TermMonad m sig) => m a
cutfail = send Cut

call :: (Subset Cut sig, TermMonad m sig) => m a -> m a
call m = send (Call m pure)

cut :: (Alternative m, Subset Cut sig, TermMonad m sig) => m ()
cut = skip <|> cutfail

skip :: Applicative m => m ()
skip = pure ()


runCut :: TermMonad m sig => Eff (SplitH m) a -> m [a]
runCut = joinSplitH . runEff var

newtype SplitH m a = SplitH { runSplitH :: m (Maybe (a, SplitH m a)) }

joinSplitH :: Monad m => SplitH m a -> m [a]
joinSplitH = (>>= maybe (pure []) (\ (a, q) -> (a :) <$> joinSplitH q)) . runSplitH

instance Monad m => Semigroup (SplitH m a) where
  a <> b = SplitH (runSplitH a >>= maybe (runSplitH b) (\ (a', q) -> pure (Just (a', q <> b))))

instance Monad m => Monoid (SplitH m a) where
  mempty = SplitH (pure Nothing)

instance TermMonad m sig => TermAlgebra (SplitH m) (Cut :+: NonDet :+: sig) where
  var a = SplitH (pure (Just (a, SplitH (pure Nothing))))
  con = alg1 \/ alg2 \/ (wrap . con . handle [()] (fmap concat . traverse joinSplitH))
    where alg1 Cut        = mempty
          alg1 (Call m k) = m `bind` k
            where m `bind` k = SplitH (runSplitH m >>= runSplitH . maybe mempty (\ (a', q) -> k a' <> (q `bind` k)))
          alg2 Empty      = mempty
          alg2 (Choose k) = SplitH (runSplitH (k True) >>= maybe (runSplitH (k False)) (\ (a, q) -> pure (Just (a, q <> k False))))

          wrap a = SplitH (a >>= \ a' -> case a' of
            []     -> pure Nothing
            a'':as -> pure (Just (a'', wrap (pure as))))

-- runCut :: Subset NonDet sig => Eff (Cut :+: sig) a -> Eff sig a
-- runCut = go empty
--   where go :: Subset NonDet sig => Eff sig a -> Eff (Cut :+: sig) a -> Eff sig a
--         go q (Return a) = pure a <|> q
--         go q Empty      = q
--         go _ Cut        = empty
--         go q (Choose k) = go (go q (k False)) (k True)
--         go q (Call m k) = go empty m >>= go q . k
--         go q (Other op) = Eff (hfmap (go empty) op) <|> q
