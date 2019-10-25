# Migrating from Older `fused-effects` Versions

This file summarizes the changelog and extracts the pieces most relevant to bringing up `fused-effects` applications to use newer versions of the library.

## 0.5 → 1.0

* The library has been divided into two hierarchies: `Control.Effect.*` provides the effect types, and `Control.Carrier.*` provides the carrier monads needed to run said effects. Each carrier exports its relevant effect types, so it suffices to import whichever carrier is needed.
* The `Random` effect has been moved to the `fused-effects-random` package.
* The `Resource` effect has been moved to the `fused-effects-exceptions` package due to the possibility of law-violating behavior if used improperly in conjunction with `State`.
* The `Resumable` effect has been moved to `fused-effects-resumable`.
* The `Carrier` class has been renamed to `Algebra` and moved to `Control.Algebra`.
* In order to save keystrokes in the common case of `(Member eff sig, Algebra sig m)`, there now exists a `Has` constraint that covers this case and which all carrier modules reexport. Using `Has`, the above would be written `Has eff sig m`. Code that uses `Member` and `Algebra` will continue to work.

