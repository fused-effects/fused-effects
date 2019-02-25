# Releasing new versions

This checklist is designed to help @robrix remember the steps involved in making a new release.

- [ ] Determine whether the release constitutes a major, minor, or patch version bump under the PVP.
- [ ] Make a branch with the name `version-x.y.z.w`.
- [ ] Add a heading to the top of `ChangeLog.md` for the current version.
- [ ] Change the version of the package in `fused-effects.cabal`.
- [ ] Push the branch to GitHub and open a draft PR.
