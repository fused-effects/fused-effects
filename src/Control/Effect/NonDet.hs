{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, NonDetC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Monad.Base
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce
import Prelude hiding (fail)

data NonDet (m :: * -> *) k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance HFunctor NonDet where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect NonDet where
  handle _     _       Empty      = Empty
  handle state handler (Choose k) = Choose (handler . (<$ state) . k)


-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: (Alternative f, Applicative m) => NonDetC m a -> m (f a)
runNonDet (NonDetC m) = m (fmap . (<|>) . pure) (pure empty)

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving two parameters: a function to combine each solution with the rest of the solutions, and an action to run when no results are produced.
    runNonDetC :: forall b . (a -> m b -> m b) -> m b -> m b
  }
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ cons -> cons a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ cons ->
    f (\ f' -> a (cons . f'))
  {-# INLINE (<*>) #-}

instance Alternative (NonDetC m) where
  empty = NonDetC (\ _ nil -> nil)
  {-# INLINE empty #-}
  NonDetC l <|> NonDetC r = NonDetC $ \ cons -> l cons . r cons
  {-# INLINE (<|>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ cons ->
    a (\ a' -> runNonDetC (f a') cons)
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (NonDetC m) where
  fail s = NonDetC (\ _ _ -> fail s)
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = NonDetC (\ cons nil -> liftIO io >>= flip cons nil)
  {-# INLINE liftIO #-}

instance MonadPlus (NonDetC m)

instance MonadTrans NonDetC where
  lift m = NonDetC (\ cons nil -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (NonDetC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L Empty)      = empty
  eff (L (Choose k)) = k True <|> k False
  eff (R other)      = NonDetC $ \ cons nil -> eff (handle [()] (fmap concat . traverse runNonDet) other) >>= foldr cons nil
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Foldable (asum)
