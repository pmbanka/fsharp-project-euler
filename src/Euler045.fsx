open System

let hexagonal n = n*(2L*n-1L)

let isInteger f = f - float (int f) = 0.0

let isTriangular (n:int64) =
    sqrt (8.0 * float n + 1.0) 
    |> isInteger

let isPentagonal (n:int64) =
    (sqrt (24.0 * float n + 1.0) + 1.0) / 6.0
    |> isInteger

Seq.initInfinite (fun x -> int64 x + 144L)
|> Seq.map hexagonal
|> Seq.find (fun x -> isTriangular x && isPentagonal x)