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

let isTruncatable allPrimes prime =
    
    ()

primesBelow 1000000