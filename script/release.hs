#!/usr/bin/env cabal exec runghc -- -package=fused-effects
{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
-- Usage: script/release.hs
-- Walks you through the release checklist
module Main (main) where

import Control.Algebra
import Control.Monad.IO.Class
import GHC.Generics (Generic1)

main :: IO ()
main = runTeletype $ sequence_ steps

steps =
  [ step "Determine whether the release constitutes a major, minor, or patch version bump under the PVP."
  , step "Make a branch with the name `version-x.y.z.w`."
  , step "Add a heading to the top of `ChangeLog.md` for the current version."
  , step "Change the version of the package in `fused-effects.cabal`."
  , step "Push the branch to GitHub and open a draft PR. Double-check the changes, comparing against a previous release PR, e.g. https://github.com/fused-effects/fused-effects/pull/80. When satisfied, mark the PR as ready for review, and request a review from a collaborator."
  , step "Build locally using `cabal v2-build`, then collect the sources and docs with `cabal v2-sdist` and `cabal v2-haddock --haddock-for-hackage`, respectively. Note the paths to the tarballs in the output of these commands."
  , step "Publish a candidate release to Hackage with `cabal upload dist-newstyle/sdist/fused-effects-x.y.z.w.tar.gz` and `cabal upload --documentation dist-newstyle/fused-effects-x.y.z.w-docs.tar.gz`. Add a link to the candidate release in a comment on the PR."
  , step "Once the PR has been approved and you’re satisfied with the candidate release, merge the PR. Publish the release to Hackage by running the above commands with the addition of `--publish`."
  , step "Locally, check out `master` and pull the latest changes to your working copy. Make a new tag, e.g. `git tag x.y.z.w`."
  , step "Push tags to GitHub using `git push --tags`."
  ]

step :: Has Teletype sig m => String -> m ()
step s = write s <* prompt "press enter to continue:"


data Teletype m k
  = Prompt String (String -> m k)
  | Write String (m k)
  deriving (Functor, Generic1)

instance Effect Teletype

prompt :: Has Teletype sig m => String -> m String
prompt s = send (Prompt s pure)

write :: Has Teletype sig m => String -> m ()
write s = send (Write s (pure ()))


newtype TeletypeC m a = TeletypeC { runTeletype :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeC m) where
  alg (L (Prompt s k)) = liftIO (putStr s >> getLine) >>= k
  alg (L (Write  s k)) = liftIO (putStrLn s)          >>  k
  alg (R other)        = TeletypeC (handleCoercible other)
