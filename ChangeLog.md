- Adds an `Empty` effect, modelling nondeterminism without choice ([#196](https://github.com/fused-effects/fused-effects/pull/196)).

- Adds an `EmptyC` carrier for `Empty`. ([#196](https://github.com/fused-effects/fused-effects/pull/196))

- Adds a `Choose` effect, modelling nondeterminism without failure ([#198](https://github.com/fused-effects/fused-effects/pull/198)).

- Adds a `Throw` effect, modelling failure with a value. ([#247](https://github.com/fused-effects/fused-effects/pull/247))

- Adds a `Catch` effect which can be used with `Throw` (or other kinds of failure) to model recoverable failure. ([#247](https://github.com/fused-effects/fused-effects/pull/247))

- Adds a `oneOf` function to `Control.Effect.NonDet` to provide an idiom for the common case of nondeterministically selecting from a container. ([#201](https://github.com/fused-effects/fused-effects/pull/201))

- Adds a `foldMapA` function to `Control.Effect.NonDet` mapping containers into nondeterministic computations using a supplied function. ([#204](https://github.com/fused-effects/fused-effects/pull/204))

- Defines a new `Has` constraint synonym, conveniently combining `Carrier` and `Member` constraints and used for all effect constructors. ([#217](https://github.com/fused-effects/fused-effects/pull/217))

- Allows effects to be defined and handled as sums of other effects, while still using the constructors for the component effects. This has been used to redefine `NonDet` as a sum of `Empty` and `Choose`, and `Error` as a sum of `Throw` and `Catch`. ([#199](https://github.com/fused-effects/fused-effects/pull/199), [#219](https://github.com/fused-effects/fused-effects/pull/219), [#247](https://github.com/fused-effects/fused-effects/pull/247))

- Defines `Carrier` instances for a number of types in `base`, including `Either`, `Maybe`, `[]`, and `IO`. ([#206](https://github.com/fused-effects/fused-effects/pull/206))

- Defines `Carrier` instances for a number of types in `transformers`. ([#226](https://github.com/fused-effects/fused-effects/pull/226))

- Defines an `evalFresh` handler for `Control.Carrier.Strict.FreshC`, taking the initial value. ([#267](https://github.com/fused-effects/fused-effects/pull/267))


## Backwards-incompatible changes

- Renames the `Carrier` class to `Algebra` and its `eff` method to `alg`, and moved the responsibilities of `Control.Carrier` to `Control.Algebra`. This makes the library more consistent with the literature and encourages a style of naming that focuses on morphisms rather than objects. ([#285](https://github.com/fused-effects/fused-effects/pull/285), [#294](https://github.com/fused-effects/fused-effects/pull/294))

- Removes the `HFunctor` class. Effects now need only provide an instance of `Effect`. ([#295](https://github.com/fused-effects/fused-effects/pull/295))

- Changes `handleCoercible` to invoke `send`, simplifying the idiom for reinterpreting effects. ([#295](https://github.com/fused-effects/fused-effects/pull/295))

- Defines a `handleIdentity` handler for effects which do not produce output wrapped in a functor. ([#295](https://github.com/fused-effects/fused-effects/pull/295))

- Fixes unlawful behaviour in the `Applicative` instance for `ErrorC`, which had different behaviour between `<*>` and `ap` in the presence of a divergent rhs. In order to accomplish this, `ErrorC` has been defined as a wrapper around `Control.Monad.Trans.Except.ExceptT`. ([#228](https://github.com/fused-effects/fused-effects/pull/228))

- Improves the performance of `runInterpret` using reflection, changing its signature slightly ([#193](https://github.com/fused-effects/fused-effects/pull/193), h/t [@ocharles](https://github.com/ocharles)).

- Removes `Control.Effect.Random` (and the dependencies on `random` & `MonadRandom`) in favour of a new [`fused-effects-random` package](https://github.com/fused-effects/fused-effects-random) ([#200](https://github.com/fused-effects/fused-effects/pull/200)).

- Removes `fmap'` and `handlePure`, both deprecated in 0.5.0.0 ([#205](https://github.com/fused-effects/fused-effects/pull/205)).

- Redefines `NonDetC` as a Church-encoded binary tree instead of a Church-encoded list ([#197](https://github.com/fused-effects/fused-effects/pull/197)).

- Removes the `OnceC` carrier for `Cull` effects, replacing it with the composition of `CullC` on some other `Alternative` carrier, e.g. `NonDetC` ([#204](https://github.com/fused-effects/fused-effects/pull/204)).

- Moves all the carriers into their own modules in the `Control.Carrier` namespace. Several have also been renamed, e.g. the various `Trace` carriers are all named `TraceC` within their separate modules, and should be imported qualified if disambiguation is required. This simplifies naming schemes, and ensures that the choice of e.g. strict or lazy carrier is always made consciously and expliclty, instead of defaulting to whichever is exported by the effect module ([#204](https://github.com/fused-effects/fused-effects/pull/204)).

- Removes the re-export of `Member` from all carrier modules, re-exporting `Has` in its place. `Has` constraints should generally be used instead, and specialist cases can import `Control.Effect.Sum` for `Member`. ([#217](https://github.com/fused-effects/fused-effects/pull/217))

- Redesigns & renames the handlers for church-encoded nondeterminism carriers to standardize naming and usage patterns. ([#207](https://github.com/fused-effects/fused-effects/pull/207))
  - The primary handlers (`runChoose`, `runNonDet`, `runCut`, `runCull`) take multiple continuations.
  - Handlers which return an `Alternative` are suffixed with `A`, e.g. `runNonDetA`.
  - Handlers which return a `Monoid` are suffixed with `M`, e.g. `runNonDetM`.
  - Handlers which return a `Semigroup` are suffixed with `S`, e.g. `runChooseS`.

- Removes `InterposeC` & `runInterpose` due to their inefficiency. They can be replaced with use of `InterpretC`/`runInterpret` for the desired effect. ([#223](https://github.com/fused-effects/fused-effects/pull/223))

- Removes `prj` from `Member`, as it was only used in `InterposeC` (see above), and was generally inadvisable due to its lack of modularity. ([#223](https://github.com/fused-effects/fused-effects/pull/223))

- Removes the `Resource` effect and carrier. Both have been relocated to `fused-effects-exceptions`. ([#268](https://github.com/fused-effects/fused-effects/pull/268))

- Redefines `Fail` as a synonym for `Throw String`. ([#247](https://github.com/fused-effects/fused-effects/pull/247))

- Removes `Resumable` and its carriers. Both have been relocated to `fused-effects-resumable`; they can also be usefully and flexibly replaced by arbitrary effects, `Lift`, and `InterpretC`. ([#269](https://github.com/fused-effects/fused-effects/pull/269))

- Changes `Control.Carrier.Fresh.Strict.runFresh` to take and return the initial & final values, respectively, allowing for safer operation. ([#267](https://github.com/fused-effects/fused-effects/pull/267))

- Removes `resetFresh`, as it was unsafe. Greater safety _and_ control over the generation of fresh values can be obtained by use of `runFresh`. ([#267](https://github.com/fused-effects/fused-effects/pull/267))

- Removes `PureC`; `Data.Functor.Identity.Identity` should be used instead. Note that `run` is still provided as a convenient synonym for `runIdentity`. ([#307](https://github.com/fused-effects/fused-effects/pull/307))

- Removes the `Pure` effect. It’s unlikely that this will require changes, as `Pure` had no operations, but `Lift Identity` should be used instead. ([#307](https://github.com/fused-effects/fused-effects/pull/307))

- Redefines the `Lift` effect, allowing inner contexts to run actions in outer contexts, e.g. to interoperate with `Control.Exception`. ([#306](https://github.com/fused-effects/fused-effects/pull/306))

- Removes `MonadUnliftIO` instances as they’ve been subsumed by the new definition of `Lift`. Additionally, the `ReaderT` & `IdentityT` types defined in `transformers` may be useful. ([#306](https://github.com/fused-effects/fused-effects/pull/306))


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
