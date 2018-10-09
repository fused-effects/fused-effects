{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, PolyKinds, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Cut where

import Control.Applicative (Alternative(..))
import Control.Carrier.Split
import Control.Effect

data Cut m k
  = Cut
  | forall b . Call (m b) (b -> k)

deriving instance Functor (Cut m)

instance Effect Cut where
  hfmap _ Cut        = Cut
  hfmap f (Call m k) = Call (f m) k

  handle _     Cut        = Cut
  handle state (Call m k) = Call (resume (m <$ state)) (wrap . resume . fmap k)

cutfail :: Subset Cut sig => Eff sig a
cutfail = send Cut

call :: Subset Cut sig => Eff sig a -> Eff sig a
call m = send (Call m pure)

cut :: (Subset NonDet sig, Subset Cut sig) => Eff sig ()
cut = skip <|> cutfail

skip :: Applicative m => m ()
skip = pure ()


runCut :: TermMonad m sig => Eff (Cut :+: NonDet :+: sig) a -> m [a]
runCut = joinSplitH . interpret2 alg1 alg2
  where alg1 Cut        = empty
        alg1 (Call m k) = m >>= k
        alg2 Empty      = empty
        alg2 (Choose k) = SplitH (runSplitH (k True) >>= maybe (runSplitH (k False)) (\ (a, q) -> pure (Just (a, q <|> k False))))

-- runCut :: Subset NonDet sig => Eff (Cut :+: sig) a -> Eff sig a
-- runCut = go empty
--   where go :: Subset NonDet sig => Eff sig a -> Eff (Cut :+: sig) a -> Eff sig a
--         go q (Return a) = pure a <|> q
--         go q Empty      = q
--         go _ Cut        = empty
--         go q (Choose k) = go (go q (k False)) (k True)
--         go q (Call m k) = go empty m >>= go q . k
--         go q (Other op) = Eff (hfmap (go empty) op) <|> q
