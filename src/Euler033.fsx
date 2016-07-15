open System

type Fraction = { nom : int; den : int }
with member x.AsFloat = float x.nom / float x.den

let getPossibleSimplifications f =
    let n1, n2 = Math.DivRem (f.nom, 10)
    let d1, d2 = Math.DivRem (f.den, 10)
    match n1, n2, d1, d2 with
    | a, b, c, d when a = d && b = c && a <> 0 && b <> 0 -> Some [{ nom=a; den=d }; {nom=b; den=c}]
    | a, b, c, d when a = c && a <> 0 -> Some [{ nom = b; den = d }]
    | a, b, c, d when a = d && a <> 0 -> Some [{ nom = b; den = c }]
    | a, b, c, d when b = c && b <> 0 -> Some [{ nom = a; den = d }]
    | a, b, c, d when b = d && b <> 0 -> Some [{ nom = a; den = c }]
    | _ -> None

let isCurious f =
    match getPossibleSimplifications f with
    | None -> false
    | Some simpld -> 
        simpld 
        |> Seq.exists (fun x -> Math.Abs(f.AsFloat - x.AsFloat) < 1.0e-5)

let candidates = seq {
    for denominator in 11 .. 99 do
        for nominator in 10 .. denominator-1 do
            yield { nom=nominator; den=denominator } }

let rec gcd x y =
    if y = 0 then x 
    else gcd y (x % y)

candidates 
|> Seq.where isCurious 
|> Seq.fold (fun state x -> { nom = state.nom * x.nom; den = state.den * x.den }) { nom=1; den=1 }
|> fun x -> 
    let gcdVal = gcd x.nom x.den
    x.den / gcdVal