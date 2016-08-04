open System

let pentagonal n = n*(3*n-1)/2

let isPentagonal n =
    let isInteger f = f - float (int f) = 0.0
    (sqrt (24.0*float n + 1.0) + 1.0) / 6.0
    |> isInteger

let result = 
    Seq.initInfinite (fun x -> x+1)
    |> Seq.collect (fun x -> seq { for i in x-1 .. -1 .. 1 do yield pentagonal x, pentagonal i })
    |> Seq.where (fun (n, m) -> isPentagonal (n+m) && isPentagonal (n-m))
    |> Seq.head
    |> fun pair -> abs (fst pair - snd pair)