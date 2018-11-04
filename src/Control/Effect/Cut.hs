{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
, cut
, Branch(..)
, branch
, runCut
, CutC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet
import Control.Effect.Sum

-- | 'Cut' effects are used with 'NonDet' to provide control over backtracking.
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)

deriving instance Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) k

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)

-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
--   prop> run (runNonDetOnce (runCut (cutfail <|> pure a))) == Nothing
--   prop> run (runNonDetOnce (runCut (pure a <|> cutfail))) == Just a
cutfail :: (Carrier sig m, Member Cut sig) => m a
cutfail = send Cutfail

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
--   prop> run (runNonDetOnce (runCut (call (cutfail <|> pure a) <|> pure b))) == Just b
call :: (Carrier sig m, Member Cut sig) => m a -> m a
call m = send (Call m ret)

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDetOnce (runCut (cut *> pure a <|> pure b))) == Just a
--   prop> run (runNonDetOnce (runCut (cut *> empty <|> pure a))) == Nothing
cut :: (Alternative m, Carrier sig m, Member Cut sig) => m ()
cut = pure () <|> cutfail


-- | The result of a nondeterministic branch of a computation.
data Branch a
  = Cut
  | None
  | Some a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative Branch where
  pure = Some
  {-# INLINE pure #-}

  Cut    <*> _      = Cut
  _      <*> Cut    = Cut
  None   <*> _      = None
  _      <*> None   = None
  Some f <*> Some a = Some (f a)
  {-# INLINE (<*>) #-}

instance Monad Branch where
  return = pure
  {-# INLINE return #-}

  Cut    >>= _ = Cut
  None   >>= _ = None
  Some a >>= f = f a
  {-# INLINE (>>=) #-}

-- | Case analysis for 'Branch', taking a value to use for 'Cut', a value to use for 'None', and a function to apply to the contents of 'Some'.
--
--   prop> branch Cut None Some a == a
--   prop> branch a b (applyFun f) Cut == a
--   prop> branch a b (applyFun f) None == b
--   prop> branch a b (applyFun f) (Some c) == applyFun f c
branch :: a -> a -> (b -> a) -> Branch b -> a
branch a _ _ Cut      = a
branch _ a _ None     = a
branch _ _ f (Some a) = f a


-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically 'Eff' carrying a 'NonDet' effect).
--
--   prop> run (runNonDetOnce (runCut (pure a))) == Just a
runCut :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (CutC m) a -> m a
runCut = (>>= branch empty empty pure) . runCutC . interpret

newtype CutC m a = CutC { runCutC :: m (Branch a) }

instance (Alternative m, Carrier sig m, Effect sig, Monad m) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  ret = CutC . ret . Some
  eff = CutC . handleSum (handleSum
    (eff . handle (Some ()) (branch (ret Cut) (ret None) runCutC))
    (\case
      Empty    -> ret None
      Choose k -> runCutC (k True) >>= branch (ret Cut) (runCutC (k False)) (\ a -> ret (Some a) <|> runCutC (k False))))
    (\case
      Cutfail  -> ret Cut
      Call m k -> runCutC m >>= branch (ret None) (ret None) (runCutC . k))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> instance Arbitrary a => Arbitrary (Branch a) where arbitrary = frequency [(1, pure Prune), (1, pure None), (3, Some <$> arbitrary)] ; shrink b = case b of { Some a -> Prune : None : map Some (shrink a) ; None -> [Prune] ; Prune -> [] }
