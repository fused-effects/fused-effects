# Defining new effects

Effects are a powerful mechanism for abstraction, and so defining new effects is a valuable tool for system architecture. Effects are modelled as (higher-order) functors, with an explicit continuation denoting the remainder of the computation after the effect.

It’s often helpful to start by specifying the types of the desired operations. For our example, we’re going to define a `Teletype` effect, with `read` and `write` operations, which read a string from some input and write a string to some output, respectively:

```haskell
data Teletype m k
read :: Has Teletype sig m => m String
write :: Has Teletype sig m => String -> m ()
```

Effect types must have two type parameters: `m`, denoting any computations which the effect embeds, and `k`, denoting the remainder of the computation after the effect. Note that since `Teletype` doesn’t use `m`, the compiler will infer it as being of kind `*` by default. The explicit kind annotation on `m` corrects that.

Next, we can flesh out the definition of the `Teletype` effect by providing constructors for each primitive operation:

```haskell
data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving (Functor)
```

The `Read` operation returns a `String`, and hence its continuation is represented as a function _taking_ a `String`. Thus, to continue the computation, a handler will have to provide a `String`. But since the effect type doesn’t say anything about where that `String` should come from, handlers are free to read from `stdin`, use a constant value, etc.

On the other hand, the `Write` operation returns `()`. Since a function `() -> k` is equivalent to a (non-strict) `k`, we can omit the function parameter.

Now that we have our effect datatype, we can give definitions for `read` and `write`:

```haskell
read :: Has Teletype sig m => m String
read = send (Read pure)

write :: Has Teletype sig m => String -> m ()
write s = send (Write s (pure ()))
```

This gives us enough to write computations using the `Teletype` effect. The next section discusses how to run `Teletype` computations.

## Defining effect handlers

Effects only specify actions, they don’t actually perform them. That task is left up to effect handlers, typically defined as functions calling `interpret` to apply a given `Algebra` instance.

Following from the above section, we can define a carrier for the `Teletype` effect which runs the calls in an underlying `MonadIO` instance, accessed via our carrier’s own `GenericNewtypeDeriving`-derived instance:

```haskell
newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hdl sig ctx = case sig of
    L (Read    k) -> liftIO getLine      >>= hdl . (<$ ctx) . k
    L (Write s k) -> liftIO (putStrLn s) >>  hdl (k <$ ctx>)
    R other       -> TeletypeIOC (alg (runTeletypeIOC . hdl) other ctx)
```

Here, `alg` is responsible for handling effectful computations. Since the `Algebra` instance handles a sum (`:+:`) of `Teletype` and the remaining signature, `alg` has two parts: a handler for `Teletype`, and a handler for teletype effects that might be embedded inside other effects in the signature.

In this case, since the `Teletype` carrier is just a thin wrapper around the underlying computation, we pass `alg` a function to unwrap any embedded `TeletypeIOC` values by simply composing `runTeletypeIOC` onto `hom`.

That leaves `Teletype` effects themselves, which are handled with one case per constructor. Since we’re assuming the existence of a `MonadIO` instance for the underlying computation, we can use `liftIO` to inject the `getLine` and `putStrLn` actions into it, and then proceed with the continuations, unwrapping them in the process.

By convention, we also provide a `runTeletypeIO` function. For `TeletypeIOC` this just unwrapps the carrier, but for more involved carriers it might also apply some arguments. (We could also have used this name for the type’s field selector directly, at the cost of some asymmetry in its name.)

```haskell
runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC
```
