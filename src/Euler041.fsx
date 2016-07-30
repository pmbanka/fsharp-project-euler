open System

module List = 
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

module Seq =
    // From: https://gist.github.com/ruxo/a9244a6dfe5e73337261/
    let tryMax (s: 'a seq) =
        let iter = s.GetEnumerator()
        if iter.MoveNext() then
            let mutable maxValue = iter.Current
            while iter.MoveNext() do
                if maxValue < iter.Current then maxValue <- iter.Current
            Some maxValue
        else
            None

let fromDigits l =
    List.rev l 
    |> Seq.zip (Seq.initInfinite (fun x -> pown 10 x))
    |> Seq.map (fun (t1, t2) -> t1 * t2)
    |> Seq.sum

let isPrime n =
    if n <= 1 then false 
    elif n <= 3 then true
    elif n % 2 = 0 || n % 3 = 0 then false else
    let upperBound = (float >> sqrt >> ceil >> int) n
    let divisibleBy = 
        seq { 5 .. 6 .. upperBound }
        |> Seq.tryFind (fun x -> n % x = 0 || n % (x+2) = 0)
    match divisibleBy with
    | Some _ -> false
    | None -> true

let tryFindPrime max =
    let input = [1 .. max]
    List.permutations input
    |> Seq.map fromDigits
    |> Seq.where isPrime
    |> Seq.tryMax

seq { 9 .. -1 .. 1 }
|> Seq.choose tryFindPrime
|> Seq.tryHead