{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer
( Writer(..)
, tell
, runWriter
) where

import Control.Effect
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance HFunctor (Writer w) where
  hfmap _ (Tell w k) = Tell w k

instance Effect (Writer w) where
  handle state handler (Tell w k) = Tell w (handler (k <$ state))

tell :: (Subset (Writer w) sig, Effectful sig m) => w -> m ()
tell w = send (Tell w (pure ()))


runWriter :: (Effectful sig m, Monoid w) => Eff (WriterH w m) a -> m (w, a)
runWriter m = runWriterH (interpret m)

newtype WriterH w m a = WriterH { runWriterH :: m (w, a) }

instance (Monoid w, Effectful sig m) => Carrier (Writer w :+: sig) (WriterH w m) where
  gen a = WriterH (pure (mempty, a))
  alg = algW \/ (WriterH . alg . handle (mempty, ()) (uncurry runWriter'))
    where algW (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
          runWriter' w = fmap (first (w <>)) . runWriterH
