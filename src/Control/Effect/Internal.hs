{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeInType, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, runEff
, interpret
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Fail.Internal
import Control.Effect.Lift.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Random.Internal
import Control.Effect.Sum
import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Prelude hiding (fail)

newtype Eff carrier a = Eff { unEff :: forall x . (a -> carrier x) -> carrier x }

runEff :: (a -> carrier b) -> Eff carrier a -> carrier b
runEff = flip unEff
{-# INLINE runEff #-}

interpret :: Carrier sig carrier => Eff carrier a -> carrier a
interpret = runEff ret
{-# INLINE interpret #-}

instance Functor (Eff carrier) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Eff carrier) where
  pure a = Eff ($ a)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | Run computations nondeterministically.
--
--   prop> run (runNonDet empty) == []
--   prop> run (runNonDet empty) == Nothing
--
--   prop> run (runNonDet (pure a <|> pure b)) == [a, b]
--   prop> run (runNonDet (pure a <|> pure b)) == Just a
--
--   Associativity:
--
--   prop> run (runNonDet ((pure a <|> pure b) <|> pure c)) == (run (runNonDet (pure a <|> (pure b <|> pure c))) :: [Integer])
--   prop> run (runNonDet ((pure a <|> pure b) <|> pure c)) == (run (runNonDet (pure a <|> (pure b <|> pure c))) :: Maybe Integer)
--
--   Left-identity:
--
--   prop> run (runNonDet (empty <|> pure b)) == (run (runNonDet (pure b)) :: [Integer])
--   prop> run (runNonDet (empty <|> pure b)) == (run (runNonDet (pure b)) :: Maybe Integer)
--
--   Right-identity:
--
--   prop> run (runNonDet (pure a <|> empty)) == (run (runNonDet (pure a)) :: [Integer])
--   prop> run (runNonDet (pure a <|> empty)) == (run (runNonDet (pure a)) :: Maybe Integer)
instance (Member NonDet sig, Carrier sig carrier) => Alternative (Eff carrier) where
  empty = send Empty
  {-# INLINE empty #-}

  l <|> r = send (Choose (\ c -> if c then l else r))
  {-# INLINE (<|>) #-}

instance Monad (Eff carrier) where
  return = pure
  {-# INLINE return #-}

  Eff m >>= f = Eff (\ k -> m (runEff k . f))
  {-# INLINE (>>=) #-}

instance (Member Fail sig, Carrier sig carrier) => MonadFail (Eff carrier) where
  fail = send . Fail
  {-# INLINE fail #-}

instance (Member NonDet sig, Carrier sig carrier) => MonadPlus (Eff carrier)

instance (Member (LiftIO IO) sig, Carrier sig (carrier m)) => MonadIO (Eff (carrier m)) where
  liftIO = send . LiftIO . fmap pure
  {-# INLINE liftIO #-}

instance (Member Random sig, Carrier sig carrier) => MonadRandom (Eff carrier) where
  getRandom = send (Random ret)
  {-# INLINE getRandom #-}
  getRandomR r = send (RandomR r ret)
  {-# INLINE getRandomR #-}
  getRandomRs interval = (:) <$> getRandomR interval <*> getRandomRs interval
  {-# INLINE getRandomRs #-}
  getRandoms = (:) <$> getRandom <*> getRandoms
  {-# INLINE getRandoms #-}

instance (Member Random sig, Carrier sig carrier) => MonadInterleave (Eff carrier) where
  interleave m = send (Interleave m ret)
  {-# INLINE interleave #-}


instance Carrier sig carrier => Carrier sig (Eff carrier) where
  ret = pure
  {-# INLINE ret #-}

  eff op = Eff (\ k -> eff (hmap (runEff ret) (fmap' (runEff k) op)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Effect.NonDet
