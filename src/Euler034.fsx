open System

let factorial n =
   let mutable acc = 1
   for i in 2 .. n do
       acc <- acc * i
   acc

let factorials = [0 .. 9] |> Seq.map factorial |> Seq.toArray 

let condition n =
    let rec toDigitsRev n = seq {
        let quotient, remainder = Math.DivRem (n, 10)
        yield remainder
        if quotient <> 0 then yield! toDigitsRev quotient }
    let sumOfFactorials = 
        toDigitsRev n
        |> Seq.map (fun x -> factorials.[x])
        |> Seq.sum
    sumOfFactorials = n

let max = factorial 9 * 7
{ 3 .. max }
|> Seq.where condition
|> Seq.sum