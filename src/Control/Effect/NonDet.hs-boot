{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet
( NonDet
) where

import {-# SOURCE #-} Control.Effect.Choose (Choose)
import {-# SOURCE #-} Control.Effect.Empty (Empty)
import Control.Effect.Sum

type NonDet = Empty :+: Choose
