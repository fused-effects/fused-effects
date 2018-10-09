{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.Writer where

import Control.Applicative (liftA2)
import Control.Carrier
import Data.Bifunctor (first)

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

  suspend = WriterH (pure (mempty, (mempty, ())))

  resume (w, m) = first (w <>) <$> runWriterH m

  wrap = WriterH
