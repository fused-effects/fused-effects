#!/usr/bin/env cabal exec runghc -- -package=fused-effects
{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
-- Usage: script/release.hs
-- Walks you through the release checklist
module Main (main) where

import Control.Algebra
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import System.Process

main :: IO ()
main = runTeletype . runLive $ do
  manual "Determine whether the release constitutes a major, minor, or patch version bump under the PVP."
  manual "Make a branch with the name `version-x.y.z.w`."
  manual "Add a heading to the top of `ChangeLog.md` for the current version."
  manual "Change the version of the package in `fused-effects.cabal`."
  manual "Push the branch to GitHub and open a draft PR. Double-check the changes, comparing against a previous release PR, e.g. https://github.com/fused-effects/fused-effects/pull/80. When satisfied, mark the PR as ready for review, and request a review from a collaborator."
  manual "Build locally using `cabal v2-build`, then collect the sources and docs with `cabal v2-sdist` and `cabal v2-haddock --haddock-for-hackage`, respectively. Note the paths to the tarballs in the output of these commands."
  manual "Publish a candidate release to Hackage with `cabal upload dist-newstyle/sdist/fused-effects-x.y.z.w.tar.gz` and `cabal upload --documentation dist-newstyle/fused-effects-x.y.z.w-docs.tar.gz`. Add a link to the candidate release in a comment on the PR."
  manual "Once the PR has been approved and you’re satisfied with the candidate release, merge the PR. Publish the release to Hackage by running the above commands with the addition of `--publish`."
  manual "Locally, check out `master` and pull the latest changes to your working copy. Make a new tag, e.g. `git tag x.y.z.w`."
  manual "Push tags to GitHub using `git push --tags`."


data Step m k
  = Manual String (m k)
  | Command String String [String] (String -> m k)
  deriving (Functor, Generic1)

instance Effect Step

manual :: Has Step sig m => String -> m ()
manual s = send (Manual s (pure ()))

command :: Has Step sig m => String -> String -> [String] -> m String
command s cmd args = send (Command s cmd args pure)


newtype LiveC m a = LiveC { runLive :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Teletype sig m, MonadIO m) => Algebra (Step :+: sig) (LiveC m) where
  alg (L (Manual s k)) = write s >> prompt "press enter to continue:" >> write "" >> k
  alg (L (Command s cmd args k)) = do
    write s
    stdout <- liftIO (readProcess cmd args "")
    k stdout
  alg (R other) = LiveC (handleCoercible other)


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
