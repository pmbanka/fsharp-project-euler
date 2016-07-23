open System

let primesBelow n =
    let table = seq { 0 .. n } |> Seq.toArray
    table.[1] <- 0
    let rec sieve currentIdx =
        if currentIdx = table.Length-1 then
            ()
        else 
            if table.[currentIdx] <> 0 then
                for i in 2*currentIdx .. currentIdx .. table.Length-1 do
                    table.[i] <- 0
            sieve (currentIdx + 1)
    sieve 2
    table |> Seq.where ((<>) 0) |> Set.ofSeq

let trunc n = 
    seq {
        let str = n.ToString ()
        for i in 1 .. str.Length-1 do
            yield str.Substring (i, str.Length - i) 
        for i in str.Length-1 .. -1 .. 1 do
            yield str.Substring (0, i) } 
    |> Seq.map Int32.Parse

let isTruncatable allPrimes prime =
    trunc prime 
    |> Seq.forall (fun x -> Set.contains x allPrimes)

let all = primesBelow 1000000
all
|> Seq.skip 4
|> Seq.where (isTruncatable all)
|> Seq.take 11
|> Seq.sum