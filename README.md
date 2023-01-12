# A fast, flexible, fused effect system for Haskell

[![Build Status](https://github.com/fused-effects/fused-effects/actions/workflows/ci.yml/badge.svg)](https://github.com/fused-effects/fused-effects/actions) [![hackage](https://img.shields.io/hackage/v/fused-effects.svg?color=blue&style=popout)](http://hackage.haskell.org/package/fused-effects)

- [Overview][]
  - [Algebraic effects][]
  - [Higher-order effects][]
  - [Fusion][]
- [Usage][]
  - [Using built-in effects][]
  - [Invoking effects][]
  - [Running effects][]
  - [Required compiler extensions][]
  - [Defining new effects][]
  - [Defining effect handlers][]
- [Project overview][]
  - [Development][]
  - [Versioning][]
- [Benchmarks][]
- [Related work][]
  - [Contributed packages][]
  - [Projects using `fused-effects`][]
  - [Comparison to `mtl`][]
  - [Comparison to `freer-simple`][]
  - [Comparison to `polysemy`][]
  - [Comparison to `eff`][]
- [Acknowledgments][]

[Overview]: https://github.com/fused-effects/fused-effects#overview
[Algebraic effects]: https://github.com/fused-effects/fused-effects#algebraic-effects
[Higher-order effects]: https://github.com/fused-effects/fused-effects#higher-order-effects
[Fusion]: https://github.com/fused-effects/fused-effects#fusion

[Usage]: https://github.com/fused-effects/fused-effects#usage
[Using built-in effects]: https://github.com/fused-effects/fused-effects#using-built-in-effects
[Invoking effects]: https://github.com/fused-effects/fused-effects#invoking-effects
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
[Projects using `fused-effects`]: https://github.com/fused-effects/fused-effects#projects-using-fused-effects
[Comparison to `mtl`]: https://github.com/fused-effects/fused-effects#comparison-to-mtl
[Comparison to `freer-simple`]: https://github.com/fused-effects/fused-effects#comparison-to-freer-simple
[Comparison to `polysemy`]: https://github.com/fused-effects/fused-effects#comparison-to-polysemy
[Comparison to `eff`]: https://github.com/fused-effects/fused-effects#comparison-to-eff

[Acknowledgments]: https://github.com/fused-effects/fused-effects#acknowledgements


## Overview

`fused-effects` is an [effect system](https://en.wikipedia.org/wiki/Effect_system) for Haskell that values expressivity, efficiency, and rigor. It provides an encoding of [algebraic](#algebraic-effects), [higher-order](#higher-order-effects) effects, includes a library of the most common effects, and generates efficient code by [fusing](#fusion) effect handlers through computations. It is suitable for use in hobbyist, research, and industrial contexts.

Readers already familiar with effect systems may wish to start with the [usage](#usage) instead. For those interested, this [talk at Strange Loop](https://www.youtube.com/watch?v=vfDazZfxlNs) outlines the history of and motivation behind effect systems and `fused-effects` itself.

<!--
Setup, hidden from the rendered markdown.

```haskell
{-# LANGUAGE ConstraintKinds, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
module Main (module Main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = pure ()
```
-->


### Algebraic effects

In `fused-effects` and other systems with _algebraic_ (or, sometimes, _extensible_) effects, effectful programs are split into two parts: the specification (or _syntax_) of the actions to be performed, and the interpretation (or _semantics_) given to them.

In `fused-effects`, _effect types_ provide syntax and _carrier types_ provide semantics. Effect types are datatypes with one constructor for each action, invoked using the `send` builtin. Carriers are monads, with an `Algebra` instance specifying how an effect’s constructors should be interpreted. Carriers can handle more than one effect, and multiple carriers can be defined for the same effect, corresponding to different interpreters for the effect’s syntax.


### Higher-order effects

Unlike some other effect systems, `fused-effects` offers _higher-order_ (or _scoped_) effects in addition to first-order algebraic effects. In a strictly first-order algebraic effect system, operations like `local` or `catchError`, which specify some action limited to a given scope, must be implemented as interpreters, hard-coding their meaning in precisely the manner algebraic effects were designed to avoid. By specifying effects as higher-order functors, this limitation is removed, meaning that these operations admit a variety of interpretations. This means, for example, that you can introspect and redefine both the `local` and `ask` operations provided by the `Reader` effect, rather than solely `ask` (as is the case with certain formulations of algebraic effects).

As Nicolas Wu et al. showed in _[Effect Handlers in Scope][]_, this has implications for the expressiveness of effect systems. It also has the benefit of making effect handling more consistent, since scoped operations are just syntax which can be interpreted like any other, and are thus simpler to reason about.


### Fusion

In order to maximize efficiency, `fused-effects` applies _fusion laws_, avoiding the construction of intermediate representations of effectful computations between effect handlers. In fact, this is applied as far as the initial construction as well: there is no representation of the computation as a free monad parameterized by some syntax type. As such, `fused-effects` avoids the overhead associated with constructing and evaluating any underlying free or freer monad.

Instead, computations are performed in a carrier type for the syntax, typically a monad wrapping further monads, via an instance of the `Carrier` class. This carrier is specific to the effect handler selected, but since it isn’t described until the handler is applied, the separation between specification and interpretation is maintained. Computations are written against an abstract effectful signature, and only specialized to some concrete carrier when their effects are interpreted.

Since the interpretation of effects is written as a typeclass instance which `ghc` is eager to inline, performance is excellent: approximately on par with `mtl`.

Finally, since the fusion of carrier algebras occurs as a result of the selection of the carriers, it doesn’t depend on complex `RULES` pragmas, making it easy to reason about and tune.


## Usage

### Package organization

The `fused-effects` package is organized into two module hierarchies:
* those under `Control.Effect`, which provide effects and functions that invoke these effects’ capabilities.
* those under `Control.Carrier`, which provide carrier types capable of executing the effects described by a given effect type.

An additional module, `Control.Algebra`, provides the `Algebra` interface that carrier types implement to provide an interpretation of a given effect. You shouldn’t need to import it unless you’re defining your own effects.


### Invoking effects

Each module under the `Control.Effect` hierarchy provides a set of functions that invoke effects, each mapping to a constructor of the underlying effect type. These functions are similar to, but more powerful than, those provided by `mtl`. In this example, we invoke the `get` and `put` functions provided by `Control.Effect.State`, first extracting the state and then updating it with a new value:

```haskell
action1 :: Has (State String) sig m => m ()
action1 = get >>= \ s -> put ("hello, " ++ s)
```

The `Has` constraint requires a given effect (here `State`) to be present in a _signature_ (`sig`), and relates that signature to be present in a carrier type (`m`). We generally, but not always, program against an abstract carrier type, usually called `m`, as carrier types always implement the `Monad` typeclass.

To add effects to a given computation, add more `Has` constraints to the signature/carrier pair `sig` and `m`. For example, to add a `Reader` effect managing an `Int`, we would write:

```haskell
action2 :: (Has (State String) sig m, Has (Reader Int) sig m) => m ()
action2 = do
  i <- ask
  put (replicate i '!')
```


### Running effects

Effects are run with _effect handlers_, specified as functions (generally starting with `run…`) unpacking some specific monad with a `Carrier` instance. For example, we can run a `State` computation using `runState`, imported from the `Control.Carrier.State.Strict` carrier module:

```haskell
example1 :: Algebra sig m => [a] -> m (Int, ())
example1 list = runState 0 $ do
  i <- get
  put (i + length list)
```

`runState` returns a tuple of both the computed value (the `()`) and the final state (the `Int`), visible in the result of the returned computation. The `get` function is resolved with a visible type application, due to the fact that effects can contain more than one state type (in contrast with `mtl`’s `MonadState`, which limits the user to a single state type).

Since this function returns a value in some carrier `m`, effect handlers can be chained to run multiple effects. Here, we get the list to compute the length of from a `Reader` effect:

```haskell
example2 :: Algebra sig m => m (Int, ())
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

`run` is itself actually an effect handler for the `Lift Identity` effect, whose only operation is to lift a result value into a computation.

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

When defining your own effects, you may need `-XKindSignatures` if GHC cannot correctly infer the type of your constructor; see the [documentation on common errors][common] for more information about this case.

When defining carriers, you’ll need `-XTypeOperators` to declare a `Carrier` instance over (`:+:`), `-XFlexibleInstances` to loosen the conditions on the instance, `-XMultiParamTypeClasses` since `Carrier` takes two parameters, and `-XUndecidableInstances` to satisfy the coverage condition for this instance.

[common]: https://github.com/fused-effects/fused-effects/blob/master/docs/common_errors.md

The following invocation, taken from the [teletype example][], should suffice for most use or construction of effects and carriers:

[teletype example]: https://github.com/fused-effects/fused-effects/blob/master/examples/Teletype.hs

```haskell
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
```


### Defining new effects

The process of defining new effects is outlined in [`docs/defining_effects.md`][], using the classic `Teletype` effect as an example.

[`docs/defining_effects.md`]: https://github.com/fused-effects/fused-effects/blob/master/docs/defining_effects.md


## Project overview

This project builds a Haskell package named `fused-effects`. The library’s sources are in [`src`][]. Unit tests are in [`test`][], and library usage examples are in [`examples`][]. Further documentation can be found in [`docs`][].

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
cabal v2-test  # build and run the examples and tests
```

The package is available on [hackage][], and can be used by adding it to a component’s `build-depends` field in your `.cabal` file.

[hackage]: http://hackage.haskell.org


### Testing

`fused-effects` comes with a rigorous test suite. Each law or property stated in the Haddock documentation is checked using generative tests powered by the [`hedgehog`](http://hackage.haskell.org/package/hedgehog) library.


### Versioning

`fused-effects` adheres to the [Package Versioning Policy][pvp] standard.

[pvp]: https://pvp.haskell.org/faq/


## Benchmarks

To run the provided benchmark suite, use `cabal v2-bench`. You may wish to provide the `-O2` compiler option to view performance under aggressive optimizations. `fused-effects` has been [benchmarked against a number of other effect systems](https://github.com/joshvera/freemonad-benchmark). See also [@patrickt’s benchmarks](https://github.com/patrickt/effects-benchmarks).


## Related work

`fused-effects` is an encoding of higher-order algebraic effects following the recipes in _[Effect Handlers in Scope][]_ (Nicolas Wu, Tom Schrijvers, Ralf Hinze), _[Monad Transformers and Modular Algebraic Effects: What Binds Them Together][]_ (Tom Schrijvers, Maciej Piróg, Nicolas Wu, Mauro Jaskelioff), and _[Fusion for Free—Efficient Algebraic Effect Handlers][]_ (Nicolas Wu, Tom Schrijvers).

[Effect Handlers in Scope]: http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[Monad Transformers and Modular Algebraic Effects: What Binds Them Together]: http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf
[Fusion for Free—Efficient Algebraic Effect Handlers]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf


### Contributed packages

Though we aim to keep the `fused-effects` core minimal, we encourage the development of external `fused-effects`-compatible libraries. If you’ve written one that you’d like to be mentioned here, get in touch!

* [`fused-effects-lens`][felens] provides combinators to use the [`lens`][lens] library fluently inside effectful computations.
* [`fused-effects-exceptions`][exc] provides handlers for exceptions thrown in the `IO` monad.
* [`fused-effects-resumable`][] provides resumable exceptions, which can also serve as a limited form of coroutines.
* [`fused-effects-mwc-random`][] provides a performant, high-quality source of random data, as well as values from common numerical distributions.
* [`fused-effects-readline`][] provides a REPL effect that interfaces with [`haskeline`][] for its UI.
* [`fused-effects-parser`][] provides parser-combinator style effects similar to parsing libraries such as [`trifecta`][].
* [`fused-effects-optics`][] provides combinators for the [`optics`][optics] ecosystem.

[exc]: https://github.com/fused-effects/fused-effects-exceptions
[felens]: http://hackage.haskell.org/package/fused-effects-lens
[`fused-effects-mwc-random`]: https://github.com/fused-effects/fused-effects-mwc-random
[`fused-effects-resumable`]: https://github.com/fused-effects/fused-effects-resumable
[`fused-effects-readline`]: https://github.com/fused-effects/fused-effects-readline
[`haskeline`]: https://hackage.haskell.org/package/haskeline
[`fused-effects-parser`]: https://github.com/fused-effects/fused-effects-parser
[`trifecta`]: https://hackage.haskell.org/package/trifecta
[lens]: http://hackage.haskell.org/package/lens
[`fused-effects-optics`]: https://github.com/fused-effects/fused-effects-optics
[optics]: https://github.com/well-typed/optics


### Projects using `fused-effects`

* [`semantic`](http://github.com/github/semantic), a program analysis toolkit
* [`now-haskell`](http://hackage.haskell.org/package/now-haskell), a client library for AWS Lambda
* [`FOSSA`](https://serokell.io/blog/haskell-in-production-fossa), a tool for open-source risk management.


### Comparison to other effect libraries

#### Comparison to `mtl`

Like [`mtl`][], `fused-effects` provides a library of monadic effects which can be given different interpretations. In `mtl` this is done by defining new instances of the typeclasses encoding the actions of the effect, e.g. `MonadState`. In `fused-effects`, this is done by defining new instances of the `Carrier` typeclass for the effect.

Also like `mtl`, `fused-effects` allows scoped operations like `local` and `catchError` to be given different interpretations. As with first-order operations, `mtl` achieves this with a final tagless encoding via methods, whereas `fused-effects` achieves this with an initial algebra encoding via `Carrier` instances.

In addition, `mtl` and `fused-effects` are similar in that they provide instances for the monad types defined in the `transformers` package (`Control.Monad.Reader`, `Control.Monad.Writer`, etc). This means that applications using `mtl` can migrate many existing `transformers`-based monad stacks to `fused-effects` with minimal code changes. `fused-effects` provides its own hierarchy of carrier monads (under the `Control.Carrier` namespace) that provide a more fluent interface for new code, though it may be useful to use `transformers` types when working with third-party libraries.

Unlike `mtl`, effects are automatically available regardless of where they occur in the signature; in `mtl` this requires instances for all valid orderings of the transformers (O(n²) of them, in general).

Also unlike `mtl`, there can be more than one `State` or `Reader` effect in a signature. This is a tradeoff: `mtl` is able to provide excellent type inference for effectful operations like `get`, since the functional dependencies can resolve the state type from the monad type.

Unlike `fused-effects`, `mtl` provides a `ContT` monad transformer; however, it’s worth noting that many behaviours possible with delimited continuations (e.g. resumable exceptions) are directly encodable as effects.

Finally, thanks to the fusion and inlining of carriers, `fused-effects` is only marginally slower than equivalent `mtl` code (see [benchmarks](#benchmarks)).

[`mtl`]: http://hackage.haskell.org/package/mtl


#### Comparison to `freer-simple`

Like [`freer-simple`][], `fused-effects` uses an initial encoding of library- and user-defined effects as syntax which can then be given different interpretations. In `freer-simple`, this is done with a family of interpreter functions (which cover a variety of needs, and which can be extended for more bespoke needs), whereas in `fused-effects` this is done with `Carrier` instances for `newtype`s.

Unlike `fused-effects`, in `freer-simple`, scoped operations like `catchError` and `local` are implemented as interpreters, and can therefore not be given new interpretations.

Unlike `freer-simple`, `fused-effects` has relatively little attention paid to compiler error messaging, which can make common (compile-time) errors somewhat more confusing to diagnose. Similarly, `freer-simple`’s family of interpreter functions can make the job of defining new effect handlers somewhat easier than in `fused-effects`. Further, `freer-simple` provides many of the same effects as `fused-effects`, plus a coroutine effect, but minus resource management and random generation.

Finally, `fused-effects` has been [benchmarked](#benchmarks) as faster than `freer-simple`.

[`freer-simple`]: http://hackage.haskell.org/package/freer-simple


#### Comparison to `polysemy`

Like [`polysemy`](http://hackage.haskell.org/package/polysemy), `fused-effects` is a batteries-included effect system capable of scoped, reinterpretable algebraic effects.

As of GHC 8.8, `fused-effects` outperforms `polysemy`, though new effects take more code to define in `fused-effects` than `polysemy` (though the `Control.Carrier.Interpret` module provides a low-friction API for rapid prototyping of new effects). Like `freer-simple` and unlike `fused-effects`, polysemy provides custom type errors if a given effect invocation is ambigous or invalid in the current context.


#### Comparison to `eff`

[`eff`](https://github.com/lexi-lambda/eff) is similar in many ways to `fused-effects`, but is slightly more performant due to its representation of effects as typeclasses. This approach lets GHC generate better code in exchange for sacrificing the flexibility associated with effects represented as data types. `eff` also uses the `monad-control` package to lift effects between contexts rather than implementing an `Algebra`-style class itself.


### Acknowledgements

The authors of fused-effects would like to thank:

* Tom Schrijvers, Nicholas Wu, and all their collaborators for the research that led to `fused-effects`;
* Alexis King for thoughtful discussions about and suggestions regarding our methodology;
* the authors of other effect libraries, including `eff`, `polysemy`, and `capabilities`, for their exploration of the space.
