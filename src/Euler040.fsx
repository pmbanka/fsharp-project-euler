open System

let indexes = 
    seq { 0.0 .. 6.0 } 
    |> Seq.map (fun x -> Math.Pow (10.0, x))
    |> Seq.map int
    |> List.ofSeq
let max = List.last indexes

Seq.initInfinite (fun x -> string (x+1))
|> Seq.collect (fun x -> x)
|> Seq.mapi (fun idx ch -> (idx+1, ch))
|> Seq.takeWhile (fun (idx, _) -> idx <= max)
|> Seq.where (fun (idx, _) -> List.contains idx indexes)
|> Seq.map (fun (_, ch) -> Char.GetNumericValue ch |> int)
|> Seq.fold (*) 1