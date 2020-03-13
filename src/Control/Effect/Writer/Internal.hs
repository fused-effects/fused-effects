{-# LANGUAGE GADTs #-}
module Control.Effect.Writer.Internal
( Writer(..)
) where

-- | @since 0.1.0.0
data Writer w m k where
  Tell   :: w               -> Writer w m ()
  Listen :: m a             -> Writer w m (w, a)
  Censor :: (w -> w) -> m a -> Writer w m a
