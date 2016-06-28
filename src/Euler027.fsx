open System
open System.Collections.Generic

let isPrime n =
    if n < 0 then false 
    elif n % 2 = 0 then false else
    let upperBound = (float >> Math.Sqrt >> int) n + 1
    let divisibleBy = 
        seq { 3 .. 2 .. upperBound }
        |> Seq.tryFind (fun x -> n % x = 0)
    match divisibleBy with
    | Some _ -> false
    | None -> true

let memoize f =
   let dict = new Dictionary<_,_>()
   fun n ->
       match dict.TryGetValue(n) with
       | (true, v) -> v
       | _ ->
           let temp = f(n)
           dict.Add(n, temp)
           temp

let formula a b n = n*n + a*n + b

let countPrimes (a, b) =
    let isPrimeMem = memoize isPrime
    Seq.initInfinite (fun x -> x)
    |> Seq.map (formula a b)
    |> Seq.takeWhile isPrimeMem
    |> Seq.length

let input = seq {
    for a in -999 .. 999 do
        for b in -999 .. 999 do
            yield a, b }

let result = input |> Seq.maxBy countPrimes