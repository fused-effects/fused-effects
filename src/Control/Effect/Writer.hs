{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer where

import Control.Applicative (liftA2)
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

instance (Monoid w, Applicative m) => Applicative (WriterH w m) where
  pure a = WriterH (pure (mempty, a))

  WriterH f <*> WriterH a = WriterH (liftA2 (<*>) f a)

instance (Monoid w, Monad m) => Monad (WriterH w m) where
  return = pure

  WriterH a >>= f = WriterH (do
    (w1, a') <- a
    let fa = f a'
    (w2, a'') <- fa `seq` runWriterH fa
    let w = w1 <> w2
    w `seq` pure (w, a''))

instance Monoid w => Carrier ((,) w) (WriterH w) where
  joinl mf = WriterH (mf >>= runWriterH)

  suspend f = f (mempty, ())

  resume (w, m) = first (w <>) <$> runWriterH m

  wrap = WriterH

instance (Monoid w, TermMonad m sig) => TermAlgebra (WriterH w m) (Writer w :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
