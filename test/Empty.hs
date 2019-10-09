{-# LANGUAGE FlexibleContexts #-}
module Empty
( empty_annihilation
) where

import Control.Carrier
import Control.Effect.Empty

empty_annihilation :: Has Empty sig m => (m b -> m b -> prop) -> (a -> m b) -> prop
empty_annihilation (===) k = (empty >>= k) === empty
