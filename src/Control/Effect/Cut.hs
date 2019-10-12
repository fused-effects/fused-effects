{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

{- | Provides an effect to delimit backtracking in a given nondeterministic context. This effect is used in concert with 'Control.Effect.NonDet.NonDet'.

Computations that signal failure with 'cutfail' prevent backtracking within the nearest enclosing 'call'.

Predefined carriers:

* "Control.Carrier.Cut.Church"
-}

module Control.Effect.Cut
( -- * Cut effect
  Cut(..)
, cutfail
, call
, cut
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier

-- | 'Cut' effects are used with 'Choose' to provide control over backtracking.
--
-- @since 0.1.2.0
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> m k)

deriving instance Functor m => Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
--   prop> run (runNonDet (runCut (cutfail <|> pure a))) === []
--   prop> run (runNonDet (runCut (pure a <|> cutfail))) === [a]
--
-- @since 0.1.2.0
cutfail :: Has Cut sig m => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
--   prop> run (runNonDet (runCut (call (cutfail <|> pure a) <|> pure b))) === [b]
--
-- @since 0.1.2.0
call :: Has Cut sig m => m a -> m a
call m = send (Call m pure)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDet (runCut (pure a <|> cut *> pure b))) === [a, b]
--   prop> run (runNonDet (runCut (cut *> pure a <|> pure b))) === [a]
--   prop> run (runNonDet (runCut (cut *> empty <|> pure a))) === []
--
-- @since 0.1.2.0
cut :: (Alternative m, Has Cut sig m) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Carrier.Cut.Church
-- >>> import Control.Carrier.NonDet.Church
