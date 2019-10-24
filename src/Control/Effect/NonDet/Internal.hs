{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet.Internal
( NonDet
) where

import Control.Effect.Choose.Internal (Choose)
import Control.Effect.Empty.Internal (Empty)
import Control.Effect.Sum

-- | The nondeterminism effect is the composition of 'Empty' and 'Choose' effects.
--
-- @since 0.1.0.0
type NonDet = Empty :+: Choose
