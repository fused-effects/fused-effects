{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet
( NonDet
, Control.Effect.Choose.Choose (..)
, Control.Effect.Empty.Empty (..)
) where

import {-# SOURCE #-} Control.Effect.Choose
import {-# SOURCE #-} Control.Effect.Empty
import Control.Effect.Sum

type NonDet = Empty :+: Choose
