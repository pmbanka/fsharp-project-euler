open System
open System.Collections

let rec toDigitsRev n = seq {
    let div, rem = Math.DivRem (n, 10)
    yield rem
    if div <> 0 then yield! toDigitsRev div }

let isPalindromic input =
    let mutable bitArray = 0u
    input 
    |> Seq.collect toDigitsRev
    |> Seq.distinct
    |> Seq.iter (fun d -> bitArray <- bitArray ||| (1u <<< d))
    bitArray = 1022u // 3FE
    
seq { 9999 .. -1 .. 9000 }
|> Seq.map (fun x -> [x; 2*x])
|> Seq.tryFind isPalindromic
