- Adds `Monad` instances for all carrier types.
- Adds `Monad` as a superclass of `Carrier`, obviating the need for a lot of constraints.
  This is a backwards-incompatible change, as any carriers users have defined now require `Monad` instances. Note that in many cases carriers can be composed out of existing carriers and monad transformers, and thus these instances can often be derived using `-XGeneralizedNewtypeDeriving`. We also recommend compiling with `-Wredundant-constraints` as many of these can now be removed.
- Removes `ret`.
  This is a backwards-incompatible change, but `pure` or `return` can be used instead.
- Removes `Eff`, in favour of computing directly in the carriers. This enables the compiler to perform significant optimizations; see the benchmarks for details.
  This is a backwards-incompatible change to any code mentioning `Eff`. For example, handlers can simply remove the `Eff` wrapping the carrier type & any use of `interpret`. As above, we also recommend compiling with `-Wredundant-constraints` as many of these can now be removed.
- Removes `handleEither`, `handleReader`, `handleState`, `handleSum`, and `handleTraversable` in favour of composing carrier types directly.
  This is a backwards-incompatible change for `Carrier` instances making use of these helpers. Instead, carriers can be composed from other carriers and `eff` defined with `handleCoercible`; and other definitions can use `handlePure` & `handle` directly.

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
