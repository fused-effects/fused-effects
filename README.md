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

### Fusion


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
