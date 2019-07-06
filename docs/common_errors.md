# Common Errors

There are certain classes of errors when using `fused-effects`
that GHC is not particularly good at explaining. This document
is an attempt to enumerate and explicate the things that can go wrong
when using or extending this library. (It is also very much a work in
progress.)

## I'm getting kind errors when implementing a `Carrier` instance!

Given an effect datatype that doesn’t use the `m` parameter:

```haskell
data Fail m k
  = Fail String
  deriving (Functor)

newtype FailC m a = FailC { runFailC :: m (Either String a) }
```

Declaring a `Carrier` instance will fail:

```haskell
instance (Carrier sig m, Effect sig)
    => Carrier (Fail :+: sig) (FailC m) where…
```

```
• Expected kind ‘(* -> *) -> * -> *’,
    but ‘Fail :+: sig’ has kind ‘* -> * -> *’
• In the first argument of ‘Carrier’, namely ‘(Fail :+: sig)’
  In the instance declaration for
    ‘Carrier (Fail :+: sig) (FailC m)
```

This is because the `m` parameter to `Fail` is inferred to be of kind `*`:
though `Carrier` expects an `m` of kind `* -> *`, `m` is never referenced in
the definition of `Fail`, so GHC makes an understandable but incorrect inference.
An explicit kind annotation on `m` fixes the problem.

```haskell
data Fail (m :: * -> *) k
  = Fail String
  deriving (Functor)
```
