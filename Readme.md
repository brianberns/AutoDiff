# Automatic Differentiation in 38 lines of F#

[Automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) is a
technique for computing the derivative of a function in a computer program. This particular
implementation is inspired by [Automatic Differentiation in 38 lines of Haskell](https://gist.github.com/ttesmer/948df432cf46ec6db8c1e83ab59b1b21),
which is in turn inspired by [Beautiful Differentiation](http://conal.net/papers/beautiful-differentiation/beautiful-differentiation-long.pdf).

In F#, [statically resolved type parameters](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters)
(SRTP) and [operator overloading](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading)
make it possible to differentiate a function this is written almost like a plain old F# function. However, due to the limitations of F#, this implementation is much simpler
and less powerful than the original Haskell version. The F# version exists solely as a demonstration and learning tool. Do not use this code in production.

## Generic numbers

Let's start with a function that implements a simple numerical compuation in F#:

```fsharp
> let f x = sqrt (3.0 * sin x);;
val f: x: float -> float
```

We can evaluate this function to determine its value at, say, `x = 2.0`:

```fsharp
> f 2.0;;
val it: float = 1.651633216
```

What if we would also like this function to work with other numeric types, such as `float32`? As written, this won't work:

```fsharp
> f 2.0f;;

  f 2.0f;;
  --^^^^

stdin(5,3): error FS0001: This expression was expected to have type
    'float'    
but here has type
    'float32'
```

F#'s `sqrt` and `sin` functions are already both generic, so we just have a find a way to replace the `3.0` literal with
a corresponding generic version of the number 3. There's no built-in way to do this in F#, but it's not hard to write
ourselves:

```fsharp

module Generic =

    /// Converts an integer to the corresponding generic number.
    let inline fromInt n =
        assert(n > 0)
        LanguagePrimitives.GenericOne
            |> Seq.replicate n
            |> Seq.reduce (+)
```

This is an extremely impractical solution, but it works fine for our purposes. We can now rewrite our computation as:

```fsharp
let inline f x =
    sqrt (Generic.fromInt 3 * sin x)
```

We can now evaluate `f` for any type that supports `sqrt` and `sin`:

```fsharp
> f 2.0;;
val it: float = 1.651633216

> f 2.0f;;
val it: float32 = 1.651633143f
```
