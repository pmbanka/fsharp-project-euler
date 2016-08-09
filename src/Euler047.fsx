open System

let primesBelow n =
    let table = [| 0 .. n |]
    table.[1] <- 0
    let rec sieve currentIdx =
        if currentIdx = table.Length-1 then ()
        else
            if table.[currentIdx] <> 0 then
                for i in 2*currentIdx .. currentIdx .. table.Length-1 do
                    table.[i] <- 0
            sieve (currentIdx + 1)
    sieve 2
    table |> Array.where ((<>) 0)

let primes = primesBelow 1000
let max = pown (Array.last primes) 2

let countDistinctPrimeFactors primes n =
    primes
    |> Seq.takeWhile (fun p -> p < n)
    |> Seq.where (fun p -> n % p = 0)
    |> Seq.length

seq { 8 .. max }
|> Seq.windowed 4
|> Seq.where (Array.forall (fun x -> countDistinctPrimeFactors primes x = 4))
|> Seq.head