module Control.Effect.NonDet
( -- * Choose effect
  module Control.Effect.Choose
  -- * Empty effect
, module Control.Effect.Empty
  -- * Re-exports
, Alternative(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Choose hiding ((<|>), many, some)
import Control.Effect.Empty hiding (empty)
