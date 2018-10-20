# A fast, flexible effect system for Haskell

- [Overview][#overview]
- [Usage][#usage]
  - [Using built-in effects][#using-built-in-effects]
  - [Defining new effects][#defining-new-effects]
- [Related work][#related-work]
  - [Comparison to `mtl`][#comparison-to--mtl-]
  - [Comparison to `freer-simple`][#comparison-to--freer-simple-]


## Overview

An encoding of higher-order algebraic effects following the recipes in _[Effect Handlers in Scope][]_ (Nicolas Wu, Tom Schrijvers, Ralf Hinze), _[Monad Transformers and Modular Algebraic Effects: What Binds Them Together][]_ (Tom Schrijvers, Maciej Piróg, Nicolas Wu, Mauro Jaskelioff), and _[Fusion for Free—Efficient Algebraic Effect Handlers][]_ (Nicolas Wu, Tom Schrijvers).

[Effect Handlers in Scope]: http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[Monad Transformers and Modular Algebraic Effects: What Binds Them Together]: http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf
[Fusion for Free—Efficient Algebraic Effect Handlers]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf


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


### Defining new effects


## Related work

### Comparison to `mtl`

### Comparison to `freer-simple`
