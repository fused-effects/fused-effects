{-# LANGUAGE DeriveFunctor, FlexibleContexts, PolyKinds, TypeOperators #-}
module Control.Effect.Writer where

import Control.Carrier.Writer
import Control.Effect
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance Effect (Writer w) where
  hfmap _ (Tell w k) = Tell w k

  handle _ (Tell w k) = Tell w k

tell :: Subset (Writer w) sig => w -> Eff sig ()
tell w = send (Tell w (pure ()))


runWriter :: (Effect sig, Monoid w) => Eff (Writer w :+: sig) a -> Eff sig (w, a)
runWriter m = runWriterH (relay alg m)
  where alg (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
