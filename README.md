# A fast, flexible effect system for Haskell

- [Overview](#overview)
  - [Algebraic effects](#algebraic-effects)
  - [Higher-order effects](#higher-order-effects)
  - [Fusion](#fusion)
- [Usage](#usage)
  - [Using built-in effects](#using-built-in-effects)
  - [Running effects](#running-effects)
  - [Defining new effects](#defining-new-effects)
- [Related work](#related-work)
  - [Comparison to `mtl`](#comparison-to--mtl-)
  - [Comparison to `freer-simple`](#comparison-to--freer-simple-)


## Overview

`higher-order-effects` is an effect system for Haskell emphasizing expressivity and efficiency. The former is achieved by encoding [algebraic](#algebraic-effects), [higher-order](#higher-order-effects) effects, while the latter is the result of [fusing](#fusion) effect handlers all the way through computations.

Readers already familiar with effect systems may wish to start with the [usage](#usage) instead.


### Algebraic effects

In `higher-order-effects` and other systems with _algebraic_ (or, sometimes, _extensible_) effects, effectful programs are split into two parts: the specification (or _syntax_) of the actions to be performed, and the interpretation (or _semantics_) given to them. Thus, a program written using the syntax of an effect can be given different meanings by using different effect handlers.

These roles are performed by the effect and carrier types, respectively. Effects are datatypes with one constructor for each action. Carriers are generally `newtype`s, with a `Carrier` instance specifying how an effect’s constructors should be interpreted. Each carrier handles one effect, but multiple carriers can be defined for the same effect, corresponding to different interpreters for the effect’s syntax.


### Higher-order effects

Unlike most other effect systems, `higher-order-effects` offers _higher-order_ (or _scoped_) effects in addition to first-order algebraic effects. In a strictly first-order algebraic effect system, operations (like `local` or `catchError`) which specify some action limited to a given scope must be implemented as interpreters, hard-coding their meaning in precisely the manner algebraic effects were designed to avoid. By specifying effects as higher-order functors, these operations are likewise able to be given a variety of interpretations.

As Nicolas Wu et al showed in [_Effect Handlers in Scope_][], this has implications for the expressiveness of effect systems. It also has the benefit of making effect handling more consistent, since scoped operations are just syntax which can be interpreted like any other, and are thus simpler to reason about.


### Fusion

In order to maximize efficiency, `higher-order-effects` applies _fusion laws_, avoiding the construction of intermediate representations of effectful computations between effect handlers. In fact, this is applied as far as the initial construction as well: there is no representation of the computation as a free monad parameterized by some syntax type.

Instead, computations are performed in a monad named `Eff`, parameterized by the carrier type for the syntax. This carrier is specific to the effect handler selected, but since it isn’t specified until the handler is applied, the separation between specification and interpretation is maintained. Computations are written against an abstract effectful signature, and only specialized to some concrete carrier once their effects are interpreted.

Carriers needn’t be `Functor`s (let alone `Monad`s), allowing a great deal of freedom in the interpretation of effects. And since the interpretation is written as a typeclass instance which `ghc` is eager to inline, performance is excellent: on par with, or even slightly better than `mtl`.


## Usage

### Using built-in effects

Like other effect systems, effects are performed in a `Monad` extended with operations relating to the effect. In `higher-order-effects`, this is done by means of a `Member` constraint to require the effect’s presence in a _signature_, and a `Carrier` constraint to relate the signature to the `Monad`. For example, to use a `State` effect managing a `String`, one would write:

```haskell
action :: (Member (State String) sig, Carrier sig m) => m ()
```

(Additional constraints may be necessary depending on the precise operations required, e.g. to make the `Monad` methods available.)

Multiple effects can be required simply by adding their corresponding `Member` constraints to the context. For example, to add a `Reader` effect managing an `Int`, we would write:

```haskell
action :: (Member (State String) sig, Member (Reader Int) sig, Carrier sig m) => m ()
```

Different effects make different operations available; see the documentation for individual effects for more information about their operations.


### Running effects

### Defining new effects


## Related work

`higher-order-effects` is an encoding of higher-order algebraic effects following the recipes in _[Effect Handlers in Scope][]_ (Nicolas Wu, Tom Schrijvers, Ralf Hinze), _[Monad Transformers and Modular Algebraic Effects: What Binds Them Together][]_ (Tom Schrijvers, Maciej Piróg, Nicolas Wu, Mauro Jaskelioff), and _[Fusion for Free—Efficient Algebraic Effect Handlers][]_ (Nicolas Wu, Tom Schrijvers).

[Effect Handlers in Scope]: http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[Monad Transformers and Modular Algebraic Effects: What Binds Them Together]: http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf
[Fusion for Free—Efficient Algebraic Effect Handlers]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf


### Comparison to `mtl`

### Comparison to `freer-simple`
