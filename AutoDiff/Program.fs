open LanguagePrimitives

module Generic =

    let inline fromInt n =
        assert(n > 0)
        GenericOne
            |> Seq.replicate n
            |> Seq.reduce (+)

type D<'a
    when 'a : (static member Zero : 'a)
    and 'a : (static member One : 'a)> =
        D of ('a * 'a) with

    static member inline Const(x : 'a) =
        D (x, GenericZero)

    static member inline Zero =
        D.Const(GenericZero<'a>)

    static member inline One =
        D.Const(GenericOne<'a>)

    static member inline (+)(D (x, x'), D (y, y')) =
        D (x + y, x' + y')

    static member inline (*)(D (x, x'), D (y, y')) =
        D (x * y, y' * x + x' * y)

    static member inline (~-)(D (x, x')) =
        D (-x, -x')

    static member inline Sin(D (x, x')) =
        D (sin x, x' * cos x)

    static member inline Cos(D (x, x')) =
        D (cos x, x' * -(sin x))

    static member inline Sqrt(D (x, x')) =
        D (sqrt x, x' / ((Generic.fromInt 2) * sqrt x))

let inline constD x = D (x, GenericZero)
let inline idD x = D (x, GenericOne)

let inline f1 z =
    // sqrt ((GenericOne + GenericOne + GenericOne) * sin z)
    sqrt ((Generic.fromInt 3) * sin z)

let d = D (2.0, 1.0)
let moo = f1 d
printfn "%A" moo
