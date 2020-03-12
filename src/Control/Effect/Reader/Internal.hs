{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Reader.Internal
( Reader(..)
) where

-- | @since 0.1.0.0
data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)
