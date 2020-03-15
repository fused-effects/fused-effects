module Control.Carrier.Writer.Church
( -- * Writer carrier
  WriterC(..)
  -- * Writer effect
, module Control.Effect.Writer
) where

import Control.Carrier.State.Church
import Control.Effect.Writer

newtype WriterC w m a = WriterC (StateC w m a)
