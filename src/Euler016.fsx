open System

let bigPow (x:int) (y:int) =
    let bigX = bigint x
    let mutable result = 1I
    for i in 1 .. y do
        result <- result * bigX
    result
    
let sumOfDigits (n:bigint) =
    n.ToString ()
    |> Seq.map (fun ch -> int (Char.GetNumericValue ch))
    |> Seq.sum
    
sumOfDigits (bigPow 2 1000)
