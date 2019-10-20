module Control.Carrier.Pure
( run
) where

data PureC a

run :: PureC a -> a
