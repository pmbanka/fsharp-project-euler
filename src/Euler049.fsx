open System

module Integer =
    let toDigits n =
        let rec loop n = 
            seq {
                let div, rem = Math.DivRem (n, 10)
                yield rem
                if div <> 0 then yield! loop div }
        loop n |> Seq.rev |> Seq.toList

    let fromDigits = List.fold (fun state x -> state*10+x) 0

module List = 
    let rec insertions x = function
        | [] -> [[x]]
        | (y::ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | [] -> seq [[]]
        | x::xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

    let rec combinations size list = 
        match (size, list) with
        | (0, _) -> [[]]
        | (_, []) -> []
        | (n, x::xs) ->
            let withX = List.map (fun l -> x::l) (combinations (n-1) xs)
            let withoutX = combinations n xs
            withX @ withoutX
    
    let rec diffs = function
        | head1::head2::tail -> (head2-head1)::diffs(head2::tail)
        | _ -> []

module Set =
    let removeRange elements s =
        let mutable temp = s
        for e in elements do
            temp <- Set.remove e temp
        temp

module Euler = 
    let private primesBetween min max =
        let table = [| 0 .. max |]
        table.[1] <- 0
        let rec sieve currentIdx =
            if currentIdx = table.Length-1 then ()
            else
                if table.[currentIdx] <> 0 then
                    for i in 2*currentIdx .. currentIdx .. table.Length-1 do
                        table.[i] <- 0
                sieve (currentIdx + 1)
        sieve 2
        table 
        |> Seq.skipWhile (fun x -> x < min)
        |> Seq.where ((<>) 0)
        |> Seq.toList

    let private permuteDigits num =
        Integer.toDigits num
        |> List.permutations 
        |> Seq.map Integer.fromDigits
        |> Seq.distinct

    let private isArithmetic list =
        let diffs = List.diffs list
        List.forall (fun x -> x = diffs.[0]) diffs

    let private findCuriousSequences allPrimes prime =
        let primePermitations = 
            permuteDigits prime
            |> Seq.where (fun x -> Set.contains x allPrimes)
            |> Seq.toList
        let arithmeticSequences = 
            List.combinations 3 primePermitations
            |> Seq.map List.sort
            |> Seq.where isArithmetic
            |> Seq.toList
        match arithmeticSequences with
        | [] -> None
        | a -> Some a

    let findAllCurious digits =
        let rec loop primesList allPrimes result =
            match primesList with
            | [] -> result
            | hd::tl ->
                let nextAllPrimes, nextResult = 
                    match findCuriousSequences allPrimes hd with
                    | None -> allPrimes, result
                    | Some curious -> Set.removeRange (List.concat curious) allPrimes, curious::result     
                loop tl nextAllPrimes nextResult
        let primes = primesBetween (pown 10 (digits-1)) ((pown 10 digits)-1)
        let primesSet = Set.ofList primes
        loop primes primesSet []

Euler.findAllCurious 4