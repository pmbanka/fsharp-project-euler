open System

let primesBelow max =
    let table = [| 0L .. max |]
    table.[1] <- 0L
    let rec sieve currentIdx =
        if currentIdx = table.Length-1 then ()
        else
            if table.[currentIdx] <> 0L then
                for i in 2*currentIdx .. currentIdx .. table.Length-1 do
                    table.[i] <- 0L
            sieve (currentIdx + 1)
    sieve 2
    table 
    |> Seq.where ((<>) 0L)
    |> Seq.toArray

let primesSums primes =
    primes
    |> Seq.mapFold (fun state x -> (state+x, state+x)) 0L
    |> fst
    |> Seq.toArray

let solve max =
    let primesArray = primesBelow max
    let sumsArray = primesSums primesArray

    let getSum (fromIdx, toIdx) =
        match fromIdx with
        | 0 -> sumsArray.[toIdx]
        | fromIdx -> sumsArray.[toIdx] - sumsArray.[fromIdx]

    let generateIndexes len = seq {
        for i in 0 .. primesArray.Length-len do
            yield (i, i+len-1) } 

    let rec loop currLen =
        if currLen < 0 then
            "None found"
        else
            let result = 
                generateIndexes currLen
                |> Seq.takeWhile (fun x -> getSum x < max)
                |> Seq.where (fun indexes -> Array.BinarySearch (primesArray, getSum indexes) > 0)
                |> Seq.tryHead 
            match result with
            | Some (fromIdx, toIdx) -> 
                sprintf "Prime: %d; from: %d to %d; length %d" 
                    (getSum (fromIdx, toIdx)) 
                    primesArray.[fromIdx] 
                    primesArray.[toIdx] 
                    (toIdx-fromIdx+1)  
            | None -> loop (currLen-1)
    loop primesArray.Length

solve 1000000L