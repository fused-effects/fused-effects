{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module Control.Carrier.Class
( Carrier(..)
) where

import Control.Effect.Catch (Catch(..))
import Control.Effect.Choose (Choose(..))
import Control.Effect.Class
import Control.Effect.Empty (Empty(..))
import Control.Effect.Error (Error)
import Control.Effect.NonDet (NonDet)
import Control.Effect.Reader (Reader(..))
import Control.Effect.Sum ((:+:)(..))
import Control.Effect.Throw (Throw(..))
import Control.Effect.Writer (Writer(..))
import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


instance Carrier Choose NonEmpty where
  eff (Choose m) = m True S.<> m False

instance Carrier Empty Maybe where
  eff Empty = Nothing

instance Carrier (Error e) (Either e) where
  eff (L (Throw e))     = Left e
  eff (R (Catch m h k)) = either (k <=< h) k m

instance Carrier (Reader r) ((->) r) where
  eff (Ask       k) r = k r r
  eff (Local f m k) r = k (m (f r)) r

instance Carrier NonDet [] where
  eff (L Empty)      = []
  eff (R (Choose k)) = k True ++ k False

instance Monoid w => Carrier (Writer w) ((,) w) where
  eff (Tell w (w', k))    = (mappend w w', k)
  eff (Listen m k)        = uncurry k m
  eff (Censor f (w, a) k) = let (w', a') = k a in (mappend (f w) w', a')
