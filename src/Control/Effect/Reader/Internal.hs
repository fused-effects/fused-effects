{-# LANGUAGE GADTs #-}
module Control.Effect.Reader.Internal
( Reader(..)
) where

-- | @since 0.1.0.0
data Reader r m k where
  Ask   ::                    Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a
