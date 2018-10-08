{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, PolyKinds #-}
module Control.Effect.Writer where

import Control.Applicative (liftA2)
import Control.Effect
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance Effect (Writer w) where
  hfmap _ (Tell w k) = Tell w k

  handle _ (Tell w k) = Tell w k


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

instance Monoid w => Carrier (WriterH w) ((,) w) where
  joinl mf = WriterH (mf >>= runWriterH)

  suspend = WriterH (pure (mempty, (mempty, ())))

  resume (w, m) = first (w <>) <$> runWriterH m

  wrap = WriterH
