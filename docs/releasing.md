# Releasing new versions

This checklist is designed to help @robrix remember the steps involved in making a new release.

- [ ] Determine whether the release constitutes a major, minor, or patch version bump under the PVP.
- [ ] Make a branch with the name `version-x.y.z.w`.
- [ ] Add a heading to the top of `ChangeLog.md` for the current version.
- [ ] Change the version of the package in `fused-effects.cabal`.
- [ ] Push the branch to GitHub and open a draft PR. Double-check the changes, comparing against a previous release PR, e.g. https://github.com/robrix/fused-effects/pull/80. When satisfied, mark the PR as ready for review, and request a review from a collaborator.
- [ ] Build locally using `cabal new-build`, then collect the sources and docs with `cabal new-sdist` and `cabal new-haddock --haddock-for-hackage`, respectively. Note the paths to the tarballs in the output of these commands.
- [ ] Publish a candidate release to Hackage with `cabal upload dist-newstyle/sdist/fused-effects-x.y.z.w.tar.gz` and `cabal upload --documentation dist-newstyle/fused-effects-x.y.z.w-docs.tar.gz`. Add a link to the candidate release in a comment on the PR.
