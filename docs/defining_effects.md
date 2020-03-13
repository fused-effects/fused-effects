# Defining new effects

Effects are a powerful mechanism for abstraction, and so defining new effects is a valuable tool for system architecture. Effects are modelled as (higher-order) functors, with an explicit continuation denoting the remainder of the computation after the effect.

It’s often helpful to start by specifying the types of the desired operations. For our example, we’re going to define a `Teletype` effect, with `read` and `write` operations, which read a string from some input and write a string to some output, respectively:

```haskell
data Teletype (m :: Type -> Type) k
read :: Has Teletype sig m => m String
write :: Has Teletype sig m => String -> m ()
```

Effect types must have two type parameters: `m`, denoting any computations which the effect embeds, and `k`, denoting the remainder of the computation after the effect. Note that since `Teletype` doesn’t use `m`, the compiler will infer it as being of kind `*` by default. The explicit kind annotation on `m` corrects that.

Next, we can flesh out the definition of the `Teletype` effect by providing constructors for each primitive operation:

```haskell
data Teletype (m :: Type -> Type) k where
  Read  ::           Teletype m String
  Write :: String -> Teletype m ()
```

The `Read` operation returns a `String`, and hence its result type is `String`. Thus, to interpret this constructor, an algebra will have to produce a `String`. But since the effect type doesn’t say anything about where that `String` should come from, algebras are free to read from `stdin`, use a constant value, etc. By contrast, the `Write` operation takes a `String` and returns `()`.

Now that we have our effect datatype, we can give definitions for `read` and `write`:

```haskell
read :: Has Teletype sig m => m String
read = send Read

write :: Has Teletype sig m => String -> m ()
write s = send (Write s)
```

This gives us enough to write computations using the `Teletype` effect. The next section discusses how to run `Teletype` computations.


## Defining algebras

Effects only specify actions, they don’t actually specify how any actions should be performed. That task is left up to algebras, defined as `Algebra` instances.

Following from the above section, we can define a carrier for the `Teletype` effect which runs the calls in an underlying `MonadIO` instance, accessed via our carrier’s own `GenericNewtypeDeriving`-derived instance:

```haskell
newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hdl sig ctx = case sig of
    L Read      -> (<$ ctx) <$ liftIO getLine
    L (Write s) -> ctx      <$ liftIO (putStrLn s)
    R other     -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)
```

Here, `alg` is responsible for handling effectful computations. Since the `Algebra` instance handles a sum (`:+:`) of `Teletype` and the remaining signature, `alg` has two parts: a case for the `Teletype` effect (in `L`), and a case for effects in the tail of the signature (in `R`).

The `Teletype` effect is handled with a case per constructor. Since we’re assuming the existence of a `MonadIO` instance for the underlying computation, we use `liftIO` to inject the `getLine` and `putStrLn` actions into it, and simply bundle up the initial state `ctx` with the results.

Since the `Teletype` carrier is just a thin wrapper around the underlying computation, we can handle the tail of the signature by passing `alg` a function to unwrap any embedded `TeletypeIOC` values by simply composing `runTeletypeIO` onto `hdl`.
