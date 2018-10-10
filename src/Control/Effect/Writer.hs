{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer where

import Control.Effect
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance HFunctor (Writer w) where
  hfmap _ (Tell w k) = Tell w k

instance Effect (Writer w) where
  handle state handler (Tell w k) = Tell w (handler (k <$ state))

tell :: (Subset (Writer w) sig, TermMonad m sig) => w -> m ()
tell w = send (Tell w (pure ()))


runWriter :: (TermMonad m sig, Monoid w) => Eff (WriterH w m) a -> m (w, a)
runWriter m = runWriterH (interpret m)

newtype WriterH w m a = WriterH { runWriterH :: m (w, a) }

instance (Monoid w, TermMonad m sig) => Carrier (WriterH w m) (Writer w :+: sig) where
  gen a = WriterH (pure (mempty, a))
  con = alg \/ (WriterH . con . handle (mempty, ()) (uncurry runWriter'))
    where alg (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
          runWriter' w = fmap (first (w <>)) . runWriterH
