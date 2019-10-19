# A fast, flexible, fused effect system for Haskell

[![Build Status](https://travis-ci.com/fused-effects/fused-effects.svg?branch=master)](https://travis-ci.com/fused-effects/fused-effects) [![hackage](https://img.shields.io/hackage/v/fused-effects.svg?color=blue&style=popout)](http://hackage.haskell.org/package/fused-effects)

- [Overview][]
  - [Algebraic effects][]
  - [Higher-order effects][]
  - [Fusion][]
- [Usage][]
  - [Using built-in effects][]
  - [Running effects][]
  - [Required compiler extensions][]
  - [Defining new effects][]
- [Project overview][]
  - [Development][]
  - [Versioning][]
- [Benchmarks][]
- [Related work][]
  - [Contributed packages][]
  - [Comparison to `mtl`][]
  - [Comparison to `freer-simple`][]

[Overview]: https://github.com/fused-effects/fused-effects#overview
[Algebraic effects]: https://github.com/fused-effects/fused-effects#algebraic-effects
[Higher-order effects]: https://github.com/fused-effects/fused-effects#higher-order-effects
[Fusion]: https://github.com/fused-effects/fused-effects#fusion

[Usage]: https://github.com/fused-effects/fused-effects#usage
[Using built-in effects]: https://github.com/fused-effects/fused-effects#using-built-in-effects
[Running effects]: https://github.com/fused-effects/fused-effects#running-effects
[Required compiler extensions]: https://github.com/fused-effects/fused-effects#required-compiler-extensions
[Defining new effects]: https://github.com/fused-effects/fused-effects#defining-new-effects
[Defining effect handlers]: https://github.com/fused-effects/fused-effects#defining-effect-handlers

[Project overview]: https://github.com/fused-effects/fused-effects#project-overview
[Development]: https://github.com/fused-effects/fused-effects#development
[Versioning]: https://github.com/fused-effects/fused-effects#versioning

[Benchmarks]: https://github.com/fused-effects/fused-effects#benchmarks

[Related work]: https://github.com/fused-effects/fused-effects#related-work
[Contributed packages]: https://github.com/fused-effects/fused-effects#contributed-packages
[Comparison to `mtl`]: https://github.com/fused-effects/fused-effects#comparison-to-mtl
[Comparison to `freer-simple`]: https://github.com/fused-effects/fused-effects#comparison-to-freer-simple


## Overview

`fused-effects` is an effect system for Haskell emphasizing expressivity and efficiency. The former is achieved by encoding [algebraic](#algebraic-effects), [higher-order](#higher-order-effects) effects, while the latter is the result of [fusing](#fusion) effect handlers all the way through computations.

Readers already familiar with effect systems may wish to start with the [usage](#usage) instead.


### Algebraic effects

In `fused-effects` and other systems with _algebraic_ (or, sometimes, _extensible_) effects, effectful programs are split into two parts: the specification (or _syntax_) of the actions to be performed, and the interpretation (or _semantics_) given to them. Thus, a program written using the syntax of an effect can be given different meanings by using different effect handlers.

These roles are performed by the effect and carrier types, respectively. Effects are datatypes with one constructor for each action. Carriers are generally `newtype`s, with a `Carrier` instance specifying how an effect’s constructors should be interpreted. Each carrier handles one effect, but multiple carriers can be defined for the same effect, corresponding to different interpreters for the effect’s syntax.


### Higher-order effects

Unlike most other effect systems, `fused-effects` offers _higher-order_ (or _scoped_) effects in addition to first-order algebraic effects. In a strictly first-order algebraic effect system, operations (like `local` or `catchError`) which specify some action limited to a given scope must be implemented as interpreters, hard-coding their meaning in precisely the manner algebraic effects were designed to avoid. By specifying effects as higher-order functors, these operations are likewise able to be given a variety of interpretations. This means, for example, that you can introspect and redefine both the `local` and `ask` operations provided by the `Reader` effect, rather than solely `ask` (as is the case with certain formulations of algebraic effects).

As Nicolas Wu et al showed in _[Effect Handlers in Scope][]_, this has implications for the expressiveness of effect systems. It also has the benefit of making effect handling more consistent, since scoped operations are just syntax which can be interpreted like any other, and are thus simpler to reason about.


### Fusion

In order to maximize efficiency, `fused-effects` applies _fusion laws_, avoiding the construction of intermediate representations of effectful computations between effect handlers. In fact, this is applied as far as the initial construction as well: there is no representation of the computation as a free monad parameterized by some syntax type. As such, `fused-effects` avoids the overhead associated with constructing and evaluating any underlying free or freer monad.

Instead, computations are performed in a carrier type for the syntax, typically a monad wrapping further monads, via an instance of the `Carrier` class. This carrier is specific to the effect handler selected, but since it isn’t described until the handler is applied, the separation between specification and interpretation is maintained. Computations are written against an abstract effectful signature, and only specialized to some concrete carrier when their effects are interpreted.

Since the interpretation of effects is written as a typeclass instance which `ghc` is eager to inline, performance is excellent: approximately on par with `mtl`.

Finally, since the fusion of carrier algebras occurs as a result of the selection of the carriers, it doesn’t depend on complex `RULES` pragmas, making it very easy to reason about and tune.


## Usage

### Using built-in effects

Like other effect systems, effects are performed in a `Monad` extended with operations relating to the effect. In `fused-effects`, this is done by means of a `Has` constraint to require the effect’s presence in a _signature_, and to relate the signature to the _carrier_ you’re computing in. For example, to use a `State` effect managing a `String`, one would write:

```haskell
action :: Has (State String) sig m => m ()
```

Multiple effects can be required simply by adding more `Has` constraints to the context. For example, to add a `Reader` effect managing an `Int`, we would write:

```haskell
action :: (Has (State String) sig m, Has (Reader Int) sig m) => m ()
```

Different effects make different operations available; see the documentation for individual effects for more information about their operations. Note that we generally don't program against an explicit list of effect components: we take the typeclass-oriented approach, adding new constraints to `sig` as new capabilities become necessary. If you want to name and share some predefined list of effects, it's best to use the `-XConstraintKinds` extension to GHC, capturing the elements of `sig` as a type synonym of kind `Constraint`:

```haskell
type Shared sig m
  = ( Has (State String) sig m
    , Has (Reader Int)   sig m
    , Has (Writer Graph) sig m
    )

myFunction :: Shared sig m => Int -> m ()
```

### Running effects

Effects are run with _effect handlers_, specified as functions (generally starting with `run…`) unpacking some specific monad with a `Carrier` instance. For example, we can run a `State` computation using `runState`:

```haskell
example1 :: (Carrier sig m, Effect sig) => [a] -> m (Int, ())
example1 list = runState 0 $ do
  i <- get @Int
  put (i + length list)
```

`runState` returns a tuple of both the computed value (the `()`) and the final state (the `Int`), visible in the result of the returned computation. The `get` function is resolved with a visible type application, due to the fact that effects can contain more than one state type (in contrast with `mtl`'s `MonadState`, which limits the user to a single state type).

Since this function returns a value in some carrier `m`, effect handlers can be chained to run multiple effects. Here, we get the list to compute the length of from a `Reader` effect:

```haskell
example2 :: (Carrier sig m, Effect sig) => m (Int, ())
example2 = runReader "hello" . runState 0 $ do
  list <- ask
  put (length (list :: String))
```

(Note that the type annotation on `list` is necessary to disambiguate the requested value, since otherwise all the typechecker knows is that it’s an arbitrary `Foldable`. For more information, see the [comparison to `mtl`](#comparison-to-mtl).)

When all effects have been handled, a computation’s final value can be extracted with `run`:

```haskell
example3 :: (Int, ())
example3 = run . runReader "hello" . runState 0 $ do
  list <- ask
  put (length (list :: String))
```

`run` is itself actually an effect handler for the `Pure` effect, which has no operations and thus can only represent a final result value.

Alternatively, arbitrary `Monad`s can be embedded into effectful computations using the `Lift` effect. In this case, the underlying `Monad`ic computation can be extracted using `runM`. Here, we use the `MonadIO` instance for the `LiftC` carrier to lift `putStrLn` into the middle of our computation:

```haskell
example4 :: IO (Int, ())
example4 = runM . runReader "hello" . runState 0 $ do
  list <- ask
  liftIO (putStrLn list)
  put (length list)
```

(Note that we no longer need to give a type annotation for `list`, since `putStrLn` constrains the type for us.)

### Required compiler extensions

To use effects, you'll typically need `-XFlexibleContexts`.

When defining your own effects, you may need `-XKindSignatures` if GHC cannot correctly infer the type of your handler; see the [documentation on common errors][common] for more information about this case. `-XDeriveGeneric` can be used with many first-order effects to derive default implementations of `HFunctor` and `Effect`.

When defining carriers, you'll need `-XTypeOperators` to declare a `Carrier` instance over (`:+:`), `-XFlexibleInstances` to loosen the conditions on the instance, `-XMultiParamTypeClasses` since `Carrier` takes two parameters, and `-XUndecidableInstances` to satisfy the coverage condition for this instance.

[common]: https://github.com/fused-effects/fused-effects/blob/master/docs/common_errors.md

The following invocation, taken from the teletype example, should suffice for most use or construction of effects and carriers:

```haskell
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
```

### Defining new effects

The process of defining new effects is outlined in [`docs/defining_effects.md`][], using the classic `Teletype` effect as an example.

[`docs/defining_effects.md`]: https://github.com/fused-effects/fused-effects/blob/master/docs/defining_effects.md

## Project overview

This project builds a Haskell package named `fused-effects`. The library’s sources are in [`src`][], with doctests (property tests written in documentation comments) attached to most functions. Unit tests are in [`test`][], and library usage examples are in [`examples`][]. Further documentation can be found in [`docs`][].

This project adheres to the Contributor Covenant [code of conduct][]. By participating, you are expected to uphold this code.

Finally, this project is licensed under the BSD 3-clause [license][].

[`src`]: https://github.com/fused-effects/fused-effects/tree/master/src
[`test`]: https://github.com/fused-effects/fused-effects/tree/master/test
[`examples`]: https://github.com/fused-effects/fused-effects/tree/master/examples
[`docs`]: https://github.com/fused-effects/fused-effects/tree/master/docs
[code of conduct]: https://github.com/fused-effects/fused-effects/blob/master/CODE_OF_CONDUCT.md
[license]: https://github.com/fused-effects/fused-effects/blob/master/LICENSE.md


### Development

Development of `fused-effects` is typically done using `cabal v2-build`:

```shell
cabal v2-build # build the library
cabal v2-test  # build and run the examples, unit tests, and doctests
```

The package is available on [hackage][], and can be used by adding it to a component’s `build-depends` field in your `.cabal` file.

[hackage]: http://hackage.haskell.org

### Versioning

Though `fused-effects` is suitable for production work, it is currently in a pre-release state. Though we will attempt to comply with the Haskell [Package Versioning Policy][pvp] standard, we make no concrete guarantees of API stability between versions < 1.0.0.0. Once v1.0.0.0 lands, all changes will abide by the PVP MAJOR.MAJOR.MINOR.PATCH standard.

[pvp]: https://pvp.haskell.org/faq/

## Benchmarks

To run the provided benchmark suite, use `cabal v2-bench`. You may wish to provide the `-O2` compiler option to view performance under aggressive optimizations. `fused-effects` has been [benchmarked against a number of other effect systems](https://github.com/joshvera/freemonad-benchmark). See also [@patrickt’s benchmarks](https://github.com/patrickt/effects-benchmarks).


## Related work

`fused-effects` is an encoding of higher-order algebraic effects following the recipes in _[Effect Handlers in Scope][]_ (Nicolas Wu, Tom Schrijvers, Ralf Hinze), _[Monad Transformers and Modular Algebraic Effects: What Binds Them Together][]_ (Tom Schrijvers, Maciej Piróg, Nicolas Wu, Mauro Jaskelioff), and _[Fusion for Free—Efficient Algebraic Effect Handlers][]_ (Nicolas Wu, Tom Schrijvers).

[Effect Handlers in Scope]: http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[Monad Transformers and Modular Algebraic Effects: What Binds Them Together]: http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf
[Fusion for Free—Efficient Algebraic Effect Handlers]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf

### Contributed packages

Though we aim to keep the `fused-effects` core minimal, we encourage the development of external `fused-effects`-compatible libraries. If you've written one that you'd like to be mentioned here, get in touch!

* [`fused-effects-lens`][felens] provides combinators to use the [`lens`][lens] library fluently inside effectful computations.
* [`fused-effects-exceptions`][exc] provides handlers for exceptions thrown in the `IO` monad.
* [`fused-effects-resumable`][] provides resumable exceptions, which can also serve as a limited form of coroutines.
* [`fused-effects-random`][] provides a `Random` effect integrated into a `fused-effects` stack.

[exc]: https://github.com/fused-effects/fused-effects-exceptions
[felens]: http://hackage.haskell.org/package/fused-effects-lens
[`fused-effects-random`]: https://github.com/fused-effects/fused-effects-random
[`fused-effects-resumable`]: https://github.com/fused-effects/fused-effects-resumable
[lens]: http://hackage.haskell.org/package/lens

### Comparison to `mtl`

Like [`mtl`][], `fused-effects` provides a library of monadic effects which can be given different interpretations. In `mtl` this is done by defining new instances of the typeclasses encoding the actions of the effect, e.g. `MonadState`. In `fused-effects`, this is done by defining new instances of the `Carrier` typeclass for the effect.

Also like `mtl`, `fused-effects` allows scoped operations like `local` and `catchError` to be given different interpretations. As with first-order operations, `mtl` achieves this with a final tagless encoding via methods, whereas `fused-effects` achieves this with an initial algebra encoding via `Carrier` instances.

Unlike `mtl`, effects are automatically available regardless of where they occur in the signature; in `mtl` this requires instances for all valid orderings of the transformers (O(n²) of them, in general).

Also unlike `mtl`, there can be more than one `State` or `Reader` effect in a signature. This is a tradeoff: `mtl` is able to provide excellent type inference for effectful operations like `get`, since the functional dependencies can resolve the state type from the monad type. On the other hand, this behaviour can be recovered in `fused-effects` using `newtype` wrappers with phantom type parameters and helper functions, e.g.:

```haskell
newtype Wrapper s m a = Wrapper { runWrapper :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig m => Carrier sig (Wrapper s m) where …

getState :: Has (State s) sig m => Wrapper s m s
getState = get
```

Indeed, `Wrapper` can now be made an instance of `MonadState`:

```haskell
instance Has (State s) sig m => MTL.MonadState s (Wrapper s m) where
  get = Control.Effect.State.get
  put = Control.Effect.State.put
```

Thus, the approaches aren’t mutually exclusive; consumers are free to decide which approach makes the most sense for their situation.

Unlike `fused-effects`, `mtl` provides a `ContT` monad transformer; however, it’s worth noting that many behaviours possible with delimited continuations (e.g. resumable exceptions) are directly encodable as effects. Further, `fused-effects` provides a relatively large palette of these, including resumable exceptions, tracing, resource management, and others, as well as tools to define your own.

Finally, thanks to the fusion and inlining of carriers, `fused-effects` is approximately as fast as `mtl` (see [benchmarks](#benchmarks)).

[`mtl`]: http://hackage.haskell.org/package/mtl


### Comparison to `freer-simple`

Like [`freer-simple`][], `fused-effects` uses an initial encoding of library- and user-defined effects as syntax which can then be given different interpretations. In `freer-simple`, this is done with a family of interpreter functions (which cover a variety of needs, and which can be extended for more bespoke needs), whereas in `fused-effects` this is done with `Carrier` instances for `newtype`s.

(Tho note that as of `fused-effects` 0.3.1, it is possible to define handlers using `runInterpret` in a manner analogous to `freer-simple`’s `interpret`, with the caveat that its use of higher-order functions defeats the fusion and inlining of `Carrier` instances which makes `fused-effects` so efficient.)

Unlike `fused-effects`, in `freer-simple`, scoped operations like `catchError` and `local` are implemented as interpreters, and can therefore not be given new interpretations.

Unlike `freer-simple`, `fused-effects` has relatively little attention paid to compiler error messaging, which can make common (compile-time) errors somewhat more confusing to diagnose. Similarly, `freer-simple`’s family of interpreter functions can make the job of defining new effect handlers somewhat easier than in `fused-effects`. Further, `freer-simple` provides many of the same effects as `fused-effects`, plus a coroutine effect, but minus resource management and random generation.

Finally, `fused-effects` has been [benchmarked](#benchmarks) as faster than `freer-simple`.

[`freer-simple`]: http://hackage.haskell.org/package/freer-simple
