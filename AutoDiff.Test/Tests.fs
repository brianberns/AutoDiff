namespace AutoDiff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open AutoDiff

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member _.Test1() =

        let inline f1 z =
            sqrt (Generic.fromInt 3 * sin z)

        let dual1 = f1 (D (2.0, 1.0))
        Assert.AreEqual<_>(1.6516332160855343, dual1.Value)
        Assert.AreEqual<_>(-0.3779412091869595, dual1.Deriv)

        let dual1f = f1 (D (2.0f, 1.0f))
        Assert.AreEqual(1.6516332160855343f, dual1f.Value, 0.000001f)
        Assert.AreEqual(-0.3779412091869595f, dual1f.Deriv, 0.000001f)

        let inline f2 x =
            D (f1 x, (3.0 * cos x / (2.0 * sqrt (3.0 * sin x))))

        let dual2 = f2 2.0
        Assert.AreEqual(dual2, dual1)

    [<TestMethod>]
    member _.Test2() =

        let inline f x =
            let g2 = Generic.fromInt 2
            let g3 = Generic.fromInt 3
            let g4 = Generic.fromInt 4
            g2 * x**3 + g3 * x**2 + g4 * x + g2

        let valf = f (D (10.0, 1.0))
        Assert.AreEqual<_>(664.0, valf.Deriv)

    [<TestMethod>]
    member _.Test3() =

        let inline f x y z =   // f: R^3 -> R
            Generic.fromInt 2 * x**2 + Generic.fromInt 3 * y + sin z
        let dual1 = f (D (3., 1.)) (D (4., 1.)) (D (5., 1.))   // call f with dual numbers, set derivative to 1
        Assert.AreEqual<_>(29.04107572533686, dual1.Value)
        Assert.AreEqual<_>(15.283662185463227, dual1.Deriv)

        (*
        D 29.041077 15.283662
        > f x y z = (2 * x^2, 3 * y + sin z)       -- f: R^3 -> R^2
        > f (D 3 1) (D 4 1) (D 5 1) :: (Dual Float', Dual Float')
        (D 18.0 12.0,D 11.041076 3.2836623)
        *)
