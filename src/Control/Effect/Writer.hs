{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer where

import Control.Effect
import Control.Monad.Codensity
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance Effect (Writer w) where
  hfmap _ (Tell w k) = Tell w k

  handle _ (Tell w k) = Tell w k

tell :: Subset (Writer w) sig => w -> Eff sig ()
tell w = send (Tell w (pure ()))

runWriter :: (TermMonad m sig, Monoid w) => Codensity (WriterH w m) a -> m (w, a)
runWriter m = runWriterH (runCodensity var m)

newtype WriterH w m a = WriterH { runWriterH :: m (w, a) }
  deriving (Functor)

instance Monoid w => Carrier ((,) w) (WriterH w) where
  joinl mf = WriterH (mf >>= runWriterH)

  suspend f = f (mempty, ())

  resume (w, m) = first (w <>) <$> runWriterH m

  wrap = WriterH

  gen a = WriterH (pure (mempty, a))

instance (Monoid w, TermMonad m sig) => TermAlgebra (WriterH w m) (Writer w :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
