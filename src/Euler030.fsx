open System

let condition n =
    let rec toDigitsRev n = seq {
        let quotient, remainder = Math.DivRem (n, 10)
        yield remainder
        if quotient <> 0 then yield! toDigitsRev quotient }
    let sumOfPowers = 
        toDigitsRev n
        |> Seq.map (fun x -> Math.Pow(float x, 5.))
        |> Seq.map int
        |> Seq.sum
    sumOfPowers = n

let max = Math.Pow (9., 5.) * 6. |> int
{ 10 .. max }
|> Seq.where condition
|> Seq.sum
