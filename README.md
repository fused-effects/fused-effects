# Higher-order effects

A higher-order encoding of algebraic effects following the recipes in _[Effect Handlers in Scope][]_ (Nicolas Wu, Tom Schrijvers, Ralf Hinze), _[Monad Transformers and Modular Algebraic Effects: What Binds Them Together][]_ (Tom Schrijvers, Maciej Piróg, Nicolas Wu, Mauro Jaskelioff), and _[Fusion for Free—Efficient Algebraic Effect Handlers][]_ (Nicolas Wu, Tom Schrijvers).

[Effect Handlers in Scope]: http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[Monad Transformers and Modular Algebraic Effects: What Binds Them Together]: http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf
[Fusion for Free—Efficient Algebraic Effect Handlers]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf

## Common Errors

**I'm getting kind errors when implementing a `Carrier` instance!**

Given a `Teletype` data type:

```haskell
data Teletype m k
  = Read (String -> k)
  | Write String k
  deriving (Functor)

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
```

Declaring a `Carrier` instance will fail:

```haskell
instance (Monad m, Carrier sig m, Effect sig)
    => Carrier (Teletype :+: sig) (TeletypeIOC m) where…
```

```
• Expected kind ‘(* -> *) -> * -> *’,
    but ‘Teletype :+: sig’ has kind ‘* -> * -> *’
• In the first argument of ‘Carrier’, namely ‘(Teletype :+: sig)’
  In the instance declaration for
    ‘Carrier (Teletype :+: sig) (TeletypeIOC m)
```

This is because the `m` parameter to `Teletype` is inferred to be of kind `*`:
though `Carrier` expects an `m` of kind `* -> *`, `m` is never referenced in
the definition of `Teletype`, so GHC makes an understandable but incorrect inference.
An explicit kind annotation on `m` fixes the problem.

```haskell
data Teletype (m :: * -> *) k
  = Read (String -> k)
  | Write String k
  deriving (Functor)

```
