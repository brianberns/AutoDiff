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
    static member inline Sqrt(D (x, x')) =
        let two = Seq.reduce (+) [ GenericOne; GenericOne ]   // ugh
        D (sqrt x, x' / (two * sqrt x))

    static member inline Log(D (x, x')) = D (log x, x' * (GenericOne / x))
