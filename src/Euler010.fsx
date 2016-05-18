open System

let sumOfPrimesBelow n =
    let table = seq { 0L .. n } |> Seq.toArray
    table.[1] <- 0L
    let rec sieve currentIdx =
        if currentIdx = table.Length-1 then
            ()
        else 
            if table.[currentIdx] <> 0L then
                let indicesToZero = seq { 2*currentIdx .. currentIdx .. table.Length-1 }
                for i in indicesToZero do
                    table.[i] <- 0L
            sieve (currentIdx + 1)
    sieve 2
    table |> Seq.where (fun x -> x <> 0L) |> Seq.sum
    
sumOfPrimesBelow 2000000L