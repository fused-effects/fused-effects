{-# LANGUAGE DeriveFunctor, EmptyCase, FlexibleInstances, FunctionalDependencies, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Class
( Carrier(..)
) where

import {-# SOURCE #-} Control.Effect.Choose (Choose(..))
import Control.Effect.Class
import {-# SOURCE #-} Control.Effect.Empty (Empty(..))
import {-# SOURCE #-} Control.Effect.Error (Error(..))
import {-# SOURCE #-} Control.Effect.Lift (Lift(..))
import {-# SOURCE #-} Control.Effect.NonDet (NonDet)
import Control.Effect.Pure
import {-# SOURCE #-} Control.Effect.Reader (Reader(..))
import {-# SOURCE #-} Control.Effect.State (State(..))
import Control.Effect.Sum ((:+:)(..))
import {-# SOURCE #-} Control.Effect.Writer (Writer(..))
import Control.Monad ((<=<), join)
import Data.Functor.Identity
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.CPS as RWS.CPS
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


-- base

instance Carrier (Lift IO) IO where
  eff = join . unLift

instance Carrier Pure Identity where
  eff v = case v of {}

instance Carrier Choose NonEmpty where
  eff (Choose m) = m True S.<> m False

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
  eff (Tell w (w', k))    = (mappend w w', k)
  eff (Listen m k)        = uncurry k m
  eff (Censor f (w, a) k) = let (w', a') = k a in (mappend (f w) w', a')


-- transformers

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (Except.ExceptT e m) where
  eff (L (Throw e))     = Except.throwE e
  eff (L (Catch m h k)) = Except.catchE m h >>= k
  eff (R other)         = Except.ExceptT $ eff (handle (Right ()) (either (pure . Left) Except.runExceptT) other)

instance Carrier sig m => Carrier (Reader r :+: sig) (Reader.ReaderT r m) where
  eff (L (Ask       k)) = Reader.ask >>= k
  eff (L (Local f m k)) = Reader.local f m >>= k
  eff (R other)         = Reader.ReaderT $ \ r -> eff (hmap (flip Reader.runReaderT r) other)

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Reader r :+: Writer w :+: State s :+: sig) (RWS.CPS.RWST r w s m) where
  eff (L (Ask       k))      = RWS.CPS.ask >>= k
  eff (L (Local f m k))      = RWS.CPS.local f m >>= k
  eff (R (L (Tell w k)))     = RWS.CPS.tell w *> k
  eff (R (L (Listen m k)))   = RWS.CPS.listen m >>= uncurry (flip k)
  eff (R (L (Censor f m k))) = RWS.CPS.censor f m >>= k
  eff (R (R (L (Get   k))))  = RWS.CPS.get >>= k
  eff (R (R (L (Put s k))))  = RWS.CPS.put s *> k
  eff (R (R (R other)))      = RWS.CPS.rwsT $ \ r s -> unRWSTF <$> eff (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.CPS.runRWST x r s) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Reader r :+: Writer w :+: State s :+: sig) (RWS.Lazy.RWST r w s m) where
  eff (L (Ask       k))      = RWS.Lazy.ask >>= k
  eff (L (Local f m k))      = RWS.Lazy.local f m >>= k
  eff (R (L (Tell w k)))     = RWS.Lazy.tell w *> k
  eff (R (L (Listen m k)))   = RWS.Lazy.listen m >>= uncurry (flip k)
  eff (R (L (Censor f m k))) = RWS.Lazy.censor f m >>= k
  eff (R (R (L (Get   k))))  = RWS.Lazy.get >>= k
  eff (R (R (L (Put s k))))  = RWS.Lazy.put s *> k
  eff (R (R (R other)))      = RWS.Lazy.RWST $ \ r s -> unRWSTF <$> eff (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Reader r :+: Writer w :+: State s :+: sig) (RWS.Strict.RWST r w s m) where
  eff (L (Ask       k))      = RWS.Strict.ask >>= k
  eff (L (Local f m k))      = RWS.Strict.local f m >>= k
  eff (R (L (Tell w k)))     = RWS.Strict.tell w *> k
  eff (R (L (Listen m k)))   = RWS.Strict.listen m >>= uncurry (flip k)
  eff (R (L (Censor f m k))) = RWS.Strict.censor f m >>= k
  eff (R (R (L (Get   k))))  = RWS.Strict.get >>= k
  eff (R (R (L (Put s k))))  = RWS.Strict.put s *> k
  eff (R (R (R other)))      = RWS.Strict.RWST $ \ r s -> unRWSTF <$> eff (handle (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s) other)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (State.Lazy.StateT s m) where
  eff (L (Get   k)) = State.Lazy.get >>= k
  eff (L (Put s k)) = State.Lazy.put s *> k
  eff (R other)     = State.Lazy.StateT $ \ s -> swap <$> eff (handle (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (State.Strict.StateT s m) where
  eff (L (Get   k)) = State.Strict.get >>= k
  eff (L (Put s k)) = State.Strict.put s *> k
  eff (R other)     = State.Strict.StateT $ \ s -> swap <$> eff (handle (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Writer w :+: sig) (Writer.CPS.WriterT w m) where
  eff (L (Tell w k))     = Writer.CPS.tell w *> k
  eff (L (Listen m k))   = Writer.CPS.listen m >>= uncurry (flip k)
  eff (L (Censor f m k)) = Writer.CPS.censor f m >>= k
  eff (R other)          = Writer.CPS.writerT $ swap <$> eff (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.CPS.runWriterT x) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Writer w :+: sig) (Writer.Lazy.WriterT w m) where
  eff (L (Tell w k))     = Writer.Lazy.tell w *> k
  eff (L (Listen m k))   = Writer.Lazy.listen m >>= uncurry (flip k)
  eff (L (Censor f m k)) = Writer.Lazy.censor f m >>= k
  eff (R other)          = Writer.Lazy.WriterT $ swap <$> eff (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Writer w :+: sig) (Writer.Strict.WriterT w m) where
  eff (L (Tell w k))     = Writer.Strict.tell w *> k
  eff (L (Listen m k))   = Writer.Strict.listen m >>= uncurry (flip k)
  eff (L (Censor f m k)) = Writer.Strict.censor f m >>= k
  eff (R other)          = Writer.Strict.WriterT $ swap <$> eff (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) other)
