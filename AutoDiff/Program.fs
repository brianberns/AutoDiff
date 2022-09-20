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

    static member inline (/)(D (x, x'), D (y, y')) =
        failwith "moo"

    static member inline (~-)(D (x, x')) =
        D (-x, -x')

    static member inline Sin(D (x, x')) =
        D (sin x, x' * cos x)

    static member inline Cos(D (x, x')) =
        D (cos x, x' * -(sin x))

    static member inline Pow(d, n) =
        pown d n

    static member inline Sqrt(D (x, x')) =
        D (sqrt x, x' / ((Generic.fromInt 2) * sqrt x))

let inline constD x = D (x, GenericZero)
let inline idD x = D (x, GenericOne)

let inline f1 z =
    sqrt ((Generic.fromInt 3) * sin z)

let val1 = f1 (D (2.0, 1.0))
printfn "%A" val1

let val1f = f1 (D (2.0f, 1.0f))
printfn "%A" val1f

let inline f2 x =
    D (f1 x, (3.0 * cos x / (2.0 * sqrt (3.0 * sin x))))

let val2 = f2 2.0
printfn "%A" val2

let inline f x =
    let g2 = Generic.fromInt 2
    let g3 = Generic.fromInt 3
    let g4 = Generic.fromInt 4
    g2 * x**3 + g3 * x**2 + g4 * x + g2

let valf = f (D (10.0, 1.0))
printfn "%A" valf
