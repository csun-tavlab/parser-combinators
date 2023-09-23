# Nondeterministic Parser Combinators #

## Background ##

A typical signature for a deterministic parser in parser combinators is below:

```scala
type Parser[A] = (List[Elem]) => Either[String, (A, List[Elem])]
```

This encodes the idea that parsing can result in the following:

- Failure (`String`): something failed to parse, and the `String` is an error message that tells you exactly why.
- Success (`(A, List[Elem])`): something succeeded to parse, and `A` is the specific thing that was parsed in.
  Additionally, `List[Elem]` consists of the remaining tokens that still need to be parsed.

This signature is potentially problematic when it comes to OR (`|` in Scala's library), as shown below:

```scala
def or[A](p1: => Parser[A], p2: => Parser[B]): Parser[B] = {
  tokens => {
    p1(tokens) match {
      case Left(_) => p2(tokens)
      case r@Right(_, _) => r
    }
  }
}
```

As shown, OR picks the first parser that succeeded.
This effectively means that it commits to whatever choice it made.
If both `p1` and `p2` would succeed, then `p1` is deterministically chosen.
This means that this signature is fundamentally unsuitable for parsing ambiguous grammars.


## Making it Nondeterministic ##

From [the MiMIs paper](https://ieeexplore.ieee.org/document/9159050), we can use iterators to encode nondeterminism.
With this in mind, we can adjust our signature of the parser, like so:

```scala
type Parser[A] = (List[Elem]) => Iterator[(A, List[Elem])]
```

An `Iterator[A]` can be viewed as a computation that nondeterministically produces `A` values.
With this in mind, the above `Parser[A]` signature says that we will nondeterministically produce _different_ possible parses.
Failure is still accomodated (the returned iterator might have no solutions), though we lose out on the error message (this is room for improvement).
But now there can be more than a single success.

OR changes, like so:

```scala
def or[A](p1: => Parser[A], p2: => Parser[A]): Parser[A] = {
  tokens => {
    p1(tokens) ++ p2(tokens)
  }
}
```

This says to grab all the solutions from `p1`, and to stitch them together with all the solutions from `p2`.
If you only care about the first solution, then this naturally degrades to the original deterministic parser combinators.
But you can now access more than just the first solution.

(It's worth noting that `++` in Scala is call-by-name with its argument, so this won't force evaluation of `p2` until it is actually needed, so this won't create any issues with infinite recursion.)

From here, all the normal parser combinator operations can be defined.
