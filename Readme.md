# Automatic Differentiation in 38 lines of F#

[Automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) is a technique for computing the
derivative of a function in a computer program. This particular implementation is inspired by [Automatic Differentiation in 38 lines of Haskell](https://gist.github.com/ttesmer/948df432cf46ec6db8c1e83ab59b1b21),
which is in turn inspired by [Beautiful Differentiation](http://conal.net/papers/beautiful-differentiation/beautiful-differentiation-long.pdf).

[Statically resolved type parameters](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters)
(SRTP) and [operator overloading](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading)
in F# make it possible to differentiate a function that is written almost like a plain old F# function. However, due to the
limitations of F#, this implementation is much simpler and less powerful than the original Haskell version. This F# version
exists solely as a demonstration and learning tool.

## Generic numbers

Let's start with a function that implements a simple numerical computation in F#:

```fsharp
> let f x = sqrt (3.0 * sin x);;
val f: x: float -> float
```

We can evaluate this function to determine its value at, say, `x = 2.0`:

```fsharp
> f 2.0;;
val it: float = 1.651633216
```

But what if we would also like this function to work with other numeric types, such as `float32`? As currently written,
this won't work:

```fsharp
> f 2.0f;;

  f 2.0f;;
  --^^^^

stdin(5,3): error FS0001: This expression was expected to have type
    'float'    
but here has type
    'float32'
```

F#'s `sqrt` and `sin` functions are both already generic, so we just have a find a way to replace the `3.0` literal with
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

And can now evaluate `f` for any numeric type that supports `sqrt` and `sin`:

```fsharp
> f 2.0;;
val it: float = 1.651633216

> f 2.0f;;
val it: float32 = 1.651633143f
```

Note, for example, that the particular implementation of the `*` operator that is invoked will vary depending on the
numeric type.

# Dual numbers

In fact, our generic function will now work for *any* type that type implements the necessary underlying members,
such as `+`, `*`, `One`, `Sin`, and `Sqrt`. We can even write our own such type, which implements [dual numbers](https://en.wikipedia.org/wiki/Dual_number).
A dual number is a tuple where the first item is a regular value and the second item is a derivative:

```fsharp
type Dual<'a> = D of value : 'a * derivative : 'a   // simplified
```

We can perform math on dual numbers, just like regular numbers. For example, multiplication of dual numbers follows the
[product rule](https://en.wikipedia.org/wiki/Product_rule) for derivatives:

```fsharp
static member inline (*)(D (x, x'), D (y, y')) =
    D (x * y, y' * x + x' * y)
```

Once we implement all the required members, we can call our original function above with a suitable dual number:

```fsharp
> f (D (2.0, 1.0));;
val it: Dual<float> = D (1.651633216, -0.3779412092)
```

This gives us both the value of `f` and the derivative of `f` at 2.0!

More examples can be found in the unit tests.

# The 38 lines

```fsharp=
namespace AutoDiff

open LanguagePrimitives

/// A dual number, consisting of a regular value and a derivative value in tandem.
type Dual<'a
    when 'a : (static member Zero : 'a)
    and 'a : (static member One : 'a)> =

    /// A dual number.
    D of value : 'a * derivative : 'a with

    member inline d.Value = let (D(reg, _)) = d in reg
    member inline d.Deriv = let (D(_, deriv)) = d in deriv

    static member inline Const(x : 'a) = D (x, GenericZero)
    static member inline Zero = Dual.Const(GenericZero<'a>)
    static member inline One = Dual.Const(GenericOne<'a>)

    static member inline (+)(D (x, x'), D (y, y')) = D (x + y, x' + y')
    static member inline (-)(x, y) = x + (-y)
    static member inline (~-)(D (x, x')) = D (-x, -x')

    static member inline (*)(D (x, x'), D (y, y')) = D (x * y, y' * x + x' * y)
    static member inline (/)(D (x, x'), D (y, y')) =
        let deriv =
            (GenericOne / (y * y)) * (y * x' + (-x) * y')
        D (x / y, deriv)

    static member inline Sin(D (x, x')) = D (sin x, x' * cos x)
    static member inline Cos(D (x, x')) = D (cos x, x' * -(sin x))

    static member inline Pow(d, n) = pown d n
    static member inline Exp(D (x, x')) = D (exp x, x' * exp x)
    static member inline Log(D (x, x')) = D (log x, x' * (GenericOne / x))
    static member inline Sqrt(D (x, x')) =
        let two = Seq.reduce (+) [ GenericOne; GenericOne ]   // ugh
        D (sqrt x, x' / (two * sqrt x))
```
