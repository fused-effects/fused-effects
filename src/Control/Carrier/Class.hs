{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Class
( Carrier(..)
) where

import Control.Effect.Class
import Control.Effect.Choose (Choose(..))
import Control.Effect.Empty (Empty(..))
import Control.Effect.Error (Error(..))
import Control.Effect.NonDet (NonDet)
import Control.Effect.Reader (Reader(..))
import Control.Effect.Sum ((:+:)(..))
import Control.Effect.Writer (Writer(..))
import Control.Monad ((<=<))
import qualified Control.Monad.Trans.Reader as Reader

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


instance Carrier Empty Maybe where
  eff Empty = Nothing

instance Carrier (Error e) (Either e) where
  eff (Throw e)     = Left e
  eff (Catch m h k) = either (k <=< h) k m

instance Carrier (Reader r) ((->) r) where
  eff (Ask       k) r = k r r
  eff (Local f m k) r = k (m (f r)) r

instance Carrier NonDet [] where
  eff (L Empty)      = []
  eff (R (Choose k)) = k True ++ k False

instance Monoid w => Carrier (Writer w) ((,) w) where
  eff (Tell w (w', k))    = (w <> w', k)
  eff (Listen m k)        = uncurry k m
  eff (Censor f (w, a) k) = let (w', a') = k a in (f w <> w', a')


-- transformers

instance Carrier sig m => Carrier (Reader r :+: sig) (Reader.ReaderT r m) where
  eff (L (Ask       k)) = Reader.ReaderT $ \ r -> Reader.runReaderT (k r) r
  eff (L (Local f m k)) = Reader.ReaderT $ \ r -> Reader.runReaderT m (f r) >>= flip Reader.runReaderT r . k
  eff (R other)         = Reader.ReaderT $ \ r -> eff (hmap (flip Reader.runReaderT r) other)
