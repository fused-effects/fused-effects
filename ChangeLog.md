- Adds an `Empty` effect, modelling nondeterminism without choice ([#196](https://github.com/fused-effects/fused-effects/pull/196)).

- Adds a `Choose` effect, modelling nondeterminism without failure ([#198](https://github.com/fused-effects/fused-effects/pull/198)).

- Adds a `oneOf` function to `Control.Effect.NonDet` to provide an idiom for the common case of nondeterministically selecting from a container. ([#201](https://github.com/fused-effects/fused-effects/pull/201))

## Backwards-incompatible changes

- Improves the performance of `runInterpret` using reflection, changing its signature slightly ([#193](https://github.com/fused-effects/fused-effects/pull/193), h/t [@ocharles](https://github.com/ocharles)).

- Removes `Control.Effect.Random` (and the dependencies on `random` & `MonadRandom`) in favour of a new [`fused-effects-random` package](https://github.com/fused-effects/fused-effects-random) ([#200](https://github.com/fused-effects/fused-effects/pull/200)).

- Removes `fmap'` and `handlePure`, both deprecated in 0.5.0.0 ([#205](https://github.com/fused-effects/fused-effects/pull/205)).

- Redefines `NonDetC` as a Church-encoded binary tree instead of a Church-encoded list ([#197](https://github.com/fused-effects/fused-effects/pull/197)).

- Removes the `NonDet` effect, replacing it with the combination of the new `Choose` and `Empty` effects ([#199](https://github.com/fused-effects/fused-effects/pull/199)).

# v0.5.0.1

- Adds support for ghc 8.8.1.

# v0.5.0.0

- Derives `Generic1` instances for all non-existentially-quantified effect datatypes.

- Derives `Foldable` & `Traversable` instances for `:+:`.

- Defines `MonadFix` instances for all of the carriers.

- Re-exports `run`, `:+:`, and `Member` from `Control.Effect.Carrier`, reducing the number of imports needed when defining new effects.

- Re-exports `Carrier`, `Member`, and `run` from the various effect modules, reducing the number of imports needed when using existing effects.

## Backwards-incompatible changes

- Replaces `runResource` with an equivalent function that uses `MonadUnliftIO` to select the correct unlifting function (a la `withResource`, which is removed in favor of `runResource`).

- Changes the signature of `eff` from `sig m (m a) -> m a` to `sig m a -> m a`, requiring effects to hold `m k` in their continuation positions instead of merely `k`. This was done in order to improve interoperability with other presentations of higher-order syntax, e.g. `bound`; syntax used with `bound` can now be given `HFunctor` and `Carrier` instances.

  To upgrade effects used with previous versions, change any continuations from `k` to `m k`. If no existential type variables appear in the effect, you can derive `Generic1`, and thence `HFunctor` & `Effect` instances. Otherwise, implement the required instances by hand. Since continuation positions now occur in `m`, `hmap` definitions will have to apply the higher-order function to these as well.

- Adds `Functor` constraints to `hmap` and `Monad` constraints to `handle`, allowing a greater variety of instances to be defined (e.g. for recursively-nested syntax).

- Replaces the default definitions of `hmap` and `handle` with derivations based on `Generic1` instead of `Coercible`. Therefore, first-order effects wishing to derive these instances will require `Generic1` instances, presumably derived using `-XDeriveGeneric`.

- Moves `send` from `Control.Effect.Sum` to `Control.Effect.Carrier`. Likewise removes the re-export of `send` from `Control.Effect`.

- Deprecates `fmap'` in favour of `fmap`.

- Deprecates `handlePure` in favour of `hmap`.

# v0.4.0.0

## Backwards-incompatible changes

- Removes APIs deprecated in 0.3.0.0, including `Eff`, `interpret`, `ret`, and the `handle*` family of helper functions.

## Other changes

- Adds the ability to derive default instances of `HFunctor` and `Effect` for first-order effects, using the `-XDeriveAnyClass` extension.
- Adds a generic `Interpose` effect that enables arbitrary "eavesdropping" on other effects.

# 0.3.1.0

- Improved speed of `Reader`, `State`, `Writer`, and `Pure` effects by defining and inlining auxiliary `Applicative` methods.
- Adds `runInterpret` & `runInterpretState` handlers in `Control.Effect.Interpret` as a convenient way to experiment with effect handlers without defining a new carrier type and `Carrier` instance. Such handlers are somewhat less efficient than custom `Carrier`s, but allow for a smooth upgrade path when more efficiency is required.
- Added `unliftio-core` as a dependency so as to provide a blessed API for unlift-style effects and a solution to the cubic-caller problem.

# 0.3.0.0

## Backwards-incompatible changes

- Adds `Monad` as a superclass of `Carrier`, obviating the need for a lot of constraints, and `Monad` instances for all carrier types.
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

- Adds a lazy `State` carrier in `Control.Effect.State.Lazy`
- Rewrites `CutC` using an approach related to `NonDetC`, with the addition of a continuation to distinguish `empty` from `cutfail`.
- Rewrites `CullC` using `ListC` and `ReaderC`.
- Moves `OnceC` from `Control.Effect.NonDet` to `Control.Effect.Cull` to avoid cyclic dependencies.
- Adds a `runCutAll` handler for `Cut` effects, returning a collection of all results.

# 0.2.0.2

- Loosens the bounds on QuickCheck to accommodate 2.x.

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
