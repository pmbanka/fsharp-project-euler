open System

let getCollatzLength (cache:int64[]) (firstValue:int64) =
    let rec getCollatzLengthImpl n =
        let continueCollatz = function
        | x when x % 2L = 0L -> (getCollatzLengthImpl (x/2L)) + 1L
        | x -> (getCollatzLengthImpl (x*3L+1L)) + 1L
        match n with
        | 1L -> 1L
        | n when n < cache.LongLength-1L ->
            match cache.[int n] with
            | -1L -> 
                let result = continueCollatz n
                cache.[int n] <- result
                result
            | n -> n
        | n -> continueCollatz n
    getCollatzLengthImpl firstValue

let getMaxCollatzLength max =
    let cache = Array.create (10*max) -1L
    seq { 1 .. max }
    |> Seq.mapi (fun i x -> (getCollatzLength cache (int64 x), i+1))
    |> Seq.maxBy fst
    
#time
getMaxCollatzLength 1000000
#time

let getCollatzLengthNaive firstValue =
    let isEven n = n % 2 = 0
    let rec getCollatzLengthImpl n len =
        let currentLen = len + 1;
        match n with
        | 1 -> currentLen
        | n when isEven n -> getCollatzLengthImpl (n/2) currentLen
        | n -> getCollatzLengthImpl (3*n+1) currentLen
    getCollatzLengthImpl firstValue 0
