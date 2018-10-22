# Common Errors

There are certain classes of errors when using `higher-order-effects`
that GHC is not particularly good at explaining. This document
is an attempt to enumerate and explicate the things that can go wrong
when using or extending this library. (It is also very much a work in
progress.)

## I'm getting kind errors when implementing a `Carrier` instance!

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
