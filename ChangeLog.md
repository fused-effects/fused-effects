## Backwards-incompatible changes

- Adds `Monad` as a superclass of `Carrier`, obviating the need for a lot of constraints.
  This is a backwards-incompatible change, as any carriers users have defined now require `Monad` instances. Note that in many cases carriers can be composed out of existing carriers and monad transformers, and thus these instances can often be derived using `-XGeneralizedNewtypeDeriving`. We also recommend compiling with `-Wredundant-constraints` as many of these can now be removed.
- Replaces `AltC` with a new carrier, `NonDetC`, based on Ralf Hinze’s work in _[Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12)_.
  This is a backwards-incompatible change. `AltC` was equivalent to the `ListT` monad transformer, and had the same well-known limitation to commutative monads. Therefore, the elimination of `Eff` required a more durable approach.
- Removes `Branch`.
  This is a backwards-incompatible change, but was necessitated by the difficulty of implementing correct `Applicative` & `Monad` instances for carriers which used it. Carriers which were employing `Branch` internally should be reimplemented using `NonDetC` or a similar approach; see `CutC` and `CullC` for examples.
- Renames `Control.Effect.Void`, `Void`, and `VoidC` to `Control.Effect.Pure`, `Pure`, and `PureC` respectively.
  This is a backwards-incompatible change for code mentioning `VoidC`; it should be updated to reference `PureC` instead.

## Deprecations

- `Eff` and `interpret`, in favour of computing directly in the carriers. This enables the compiler to perform significant optimizations; see the benchmarks for details.
   Handlers can simply remove the `Eff` wrapping the carrier type & any use of `interpret`. As above, we also recommend compiling with `-Wredundant-constraints` as many of these can now be removed.
- `ret`, in favor of `pure` or `return`.
- `handleEither`, `handleReader`, `handleState`, `handleSum`, and `handleTraversable` in favour of composing carrier types directly.
   Carriers can be composed from other carriers and `eff` defined with `handleCoercible`; and other definitions can use `handlePure` & `handle` directly.

All deprecated APIs will be removed in the next release.

## Other changes

- Adds `Monad` instances for all carrier types.
- Rewrites `CutC` using an approach related to `NonDetC`, with the addition of a continuation to distinguish `empty` from `cutfail`.
- Rewrites `CullC` using `ListC` and `ReaderC`.
- Moves `OnceC` from `Control.Effect.NonDet` to `Control.Effect.Cull` to avoid cyclic dependencies.
- Adds a `runCutAll` handler for `Cut` effects, returning a collection of all results.

# 0.2.0.1

- Fixes the benchmarks, and builds them in CI to avoid regressing them again.

# 0.2.0.0

- Adds `listen`, `listens`, and `censor` operations to `Writer`.
- Provides explicit type parameters to `run`-style functions in `State`, `Reader`, `Writer`, and `Error`.
  This is a backwards-incompatible change for clients using these functions in combination with visible type applications.
- Adds benchmarks of `WriterC`/`VoidC` wrapped with `Eff` against their unwrapped counterparts.
- Adds `Functor`, `Applicative`, and `Monad` instances for `WriterC`.
- Adds `Functor`, `Applicative`, and `Monad` instances for `VoidC`.
- Fixes a space leak with `WriterC`.
- Removes the `Functor` constraint on `asks` and `gets`.
- Adds `bracketOnError`, `finally`, and `onException` to `Resource`.
- Adds `sendM` to `Lift`.

# 0.1.2.1

- Loosens the bounds on QuickCheck to accommodate 0.12.

# 0.1.2.0

- Adds support for ghc 8.6.2, courtesy of @jkachmar.
- Adds a `Cut` effect which adds committed choice to nondeterminism.
- Adds a `Cull` effect which adds pruning to nondeterminism.
- Adds an example of using `NonDet`, `Cut`, and a character parser effect to define parsers.
- Fixes the table of contents links in the README.

# 0.1.1.0

- Adds a `runNonDetOnce` handler which terminates immediately upon finding a solution.

# 0.1.0.0

Initial release.
