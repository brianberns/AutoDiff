namespace AutoDiff.Test

// https://gist.github.com/ttesmer/948df432cf46ec6db8c1e83ab59b1b21
// http://conal.net/papers/beautiful-differentiation/beautiful-differentiation-long.pdf
// https://en.wikipedia.org/wiki/Automatic_differentiation

open Microsoft.VisualStudio.TestTools.UnitTesting
open AutoDiff

module Generic =

    /// Converts an integer to the corresponding generic number.
    let inline fromInt n =
        assert(n > 0)
        LanguagePrimitives.GenericOne
            |> Seq.replicate n
            |> Seq.reduce (+)

[<TestClass>]
type TestClass () =

    /// Prepares to find the derivative of a function for the given input.
    let lift x = D (x, 1.)

    /// Dumb assertion workaround.
    let areEqual(expected : 't, actual : 't) =
        Assert.AreEqual<'t>(expected, actual)

    [<TestMethod>]
    member _.Basic() =

        let inline f1 z =
            sqrt (Generic.fromInt 3 * sin z)

        // expected value and derivative at x = 2
        let expectedValue = 1.6516332160855343
        let expectedDeriv = -0.3779412091869595

        let val1 = f1 2.0
        areEqual(expectedValue, val1)

        let dual1 = f1 (lift 2.0)
        areEqual(expectedValue, dual1.Value)
        areEqual(expectedDeriv, dual1.Deriv)

        let dual1f = f1 (D (2.0f, 1.0f))
        Assert.AreEqual(float32 expectedValue, dual1f.Value, 0.000001f)
        Assert.AreEqual(float32 expectedDeriv, dual1f.Deriv, 0.000001f)

        let inline f2 x =
            D (f1 x, (3.0 * cos x / (2.0 * sqrt (3.0 * sin x))))

        let dual2 = f2 2.0
        areEqual(dual2, dual1)

    [<TestMethod>]
    member _.Polynomial() =

        let inline f x =
            let g2 = Generic.fromInt 2
            let g3 = Generic.fromInt 3
            let g4 = Generic.fromInt 4
            g2 * x**3 + g3 * x**2 + g4 * x + g2

        let valf = f (lift 10.0)
        areEqual(664.0, valf.Deriv)

    /// Differentiate high-dimensional function.
    [<TestMethod>]
    member _.R3toR1() =

        let inline f x y z =   // f: R^3 -> R
            Generic.fromInt 2 * x**2 + Generic.fromInt 3 * y + sin z

        let dual = f (lift 3.) (lift 4.) (lift 5.)
        areEqual(29.04107572533686, dual.Value)
        areEqual(15.283662185463227, dual.Deriv)

    /// Differentiate high-dimensional function.
    [<TestMethod>]
    member _.R3toR2() =

        let inline f x y z =   // f: R^3 -> R^2
            Generic.fromInt 2 * x**2, Generic.fromInt 3 * y + sin z

        let dual1, dual2 = f (lift 3.) (lift 4.) (lift 5.)
        areEqual(18.0, dual1.Value)
        areEqual(12.0, dual1.Deriv)
        areEqual(11.041075725336862, dual2.Value)
        areEqual(3.283662185463226, dual2.Deriv)

    [<TestMethod>]
    member _.Reciprocal() =

        let inline f x =
            Generic.fromInt 1 / x

        let dual = f (D (10., 1.))
        areEqual(0.1, dual.Value)
        areEqual(-1./100., dual.Deriv)

    [<TestMethod>]
    member _.Division() =

        // https://calcworkshop.com/derivatives/quotient-rule/
        let inline f x =
            let g1 = Generic.fromInt 1
            let g3 = Generic.fromInt 3
            let g4 = Generic.fromInt 4
            let g5 = Generic.fromInt 5
            (g5 * x + g1) / (g3 * x - g4)

        let inline f' x =
            -23.0 / ((3. * x - 4.) ** 2.)

        let x = 10.
        let dual = f (lift x)
        areEqual(f x, dual.Value)
        areEqual(f' x, dual.Deriv)

    [<TestMethod>]
    member _.Exp() =

        let n = 2
        let inline f x =
            exp (Generic.fromInt n * x)

        let inline f' x =
            (Generic.fromInt n) * f x

        let x = 4.
        let dual = f (lift x)
        areEqual(f x, dual.Value)
        areEqual(f' x, dual.Deriv)

    [<TestMethod>]
    member _.Log() =
        let x = 4.
        let dual = log (lift x)
        areEqual(log x, dual.Value)
        areEqual(1./x, dual.Deriv)

    [<TestMethod>]
    member _.Pow() =
        let x = 4.
        let n = 3
        let dual = (lift x) ** n
        areEqual(pown x n, dual.Value)
        areEqual(float n * x * x, dual.Deriv)
