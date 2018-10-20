{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
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
import Control.Effect.Sum
import Control.Monad (liftM, ap)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

newtype Eff carrier a = Eff { unEff :: forall x . (a -> carrier x) -> carrier x }

runEff :: (a -> carrier b) -> Eff carrier a -> carrier b
runEff = flip unEff
{-# INLINE runEff #-}

interpret :: Carrier sig carrier => Eff carrier a -> carrier a
interpret = runEff handleReturn
{-# INLINE interpret #-}

instance Functor (Eff carrier) where
  fmap = liftM

instance Applicative (Eff carrier) where
  pure a = Eff ($ a)

  (<*>) = ap

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

  l <|> r = send (Choose (\ c -> if c then l else r))

instance Monad (Eff carrier) where
  return = pure

  Eff m >>= f = Eff (\ k -> m (runEff k . f))

instance (Member Fail sig, Carrier sig carrier) => MonadFail (Eff carrier) where
  fail = send . Fail

instance (Member (Lift IO) sig, Carrier sig carrier) => MonadIO (Eff carrier) where
  liftIO = send . Lift . fmap pure


instance Carrier sig carrier => Carrier sig (Eff carrier) where
  handleReturn = pure
  alg op = Eff (\ k -> alg (hmap (runEff handleReturn) (fmap' (runEff k) op)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Effect.NonDet
