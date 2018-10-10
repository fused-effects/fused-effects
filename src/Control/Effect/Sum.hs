{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect.Sum
( (:+:)(..)
, (\/)
) where

import Control.Effect.Sum.Internal

-- | Lift algebras for either side of a sum into a single algebra on sums.
(\/) :: ( sig1           m a -> b)
     -> (          sig2  m a -> b)
     -> ((sig1 :+: sig2) m a -> b)
(alg1 \/ _   ) (L op) = alg1 op
(_    \/ alg2) (R op) = alg2 op

infixr 4 \/
