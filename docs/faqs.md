# FAQs

## Why are you reimplementing types equivalent to those provided by `transformers`? Why not use `StateT` instead of defining a separate `StateC` type?

We do this when possible: `ReaderC` is just a newtype wrapper (to avoid orphan `Carrier instances`) around the `ReaderT` provided by `transformers`. Yet in many cases, the `transformers` interface is not as good as it could be: it is an old and venerable library, under significant backwards-compatibility constraints, and it shows. Defining new monads rather than reusing existing ones allows us to paper over many of the issues present in `transformers`, such as:
* Ergonomics: the order of arguments in `runState` and `runReader` makes it difficult to compose these functions without using `flip` every time.
* Correctness: the `Writer` provided by `transformers` leaks space.
* Implementation ease: the `handle` function for stateful variables is more comfortable if we reverse the order of the tuple members associated with the `StateT` transformer.
