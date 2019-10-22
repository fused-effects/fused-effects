# FAQs

## Why are you reimplementing types equivalent to those provided by `transformers`? Why not use `StateT` instead of defining a separate `StateC` type?

Were we to reuse the `transfomers` interface, we would elide a few manual instances, but we would still require a newtype wrapper so as to avoid orphan `Algebra` instances, so the degree of reuse would be minimal. Additionally, in many cases, the `transformers` interface is not as good as it could be: it is an old and venerable library, under significant backwards-compatibility constraints. Defining new monads rather than reusing existing ones allows us to paper over many of the issues present in `transformers`, such as:
* Ergonomics: the order of arguments in `runState` and `runReader` makes it difficult to compose these functions without using `flip` every time.
* Correctness: the `Writer` provided by `transformers` leaks space.
* Implementation ease: the `handle` function for stateful variables is more comfortable if we reverse the order of the tuple members associated with the `StateT` transformer.
