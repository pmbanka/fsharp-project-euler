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

let circulars n = seq {
    let str = n.ToString ()
    let strstr = str + str
    for i in 0 .. str.Length - 1 do
        yield strstr.Substring (i, str.Length) |> Int32.Parse }

let isCircularPrime allPrimes prime =
    circulars prime 
    |> Seq.forall (fun x -> Set.contains x allPrimes)

let all = primesBelow 1000000
all
|> Seq.where (isCircularPrime all)
|> Seq.length