open System

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

let (|Even|Odd|) n =
    if n % 2 = 0 then Even else Odd

let (|Prime|Composite|) n =
    if isPrime n then Prime else Composite

let findCurious () =
    let isInteger f = f - float (int f) = 0.0
    let rec loop n primes =
        match n with
        | Prime -> loop (n+1) (n::primes)
        | Composite & Odd -> 
            primes
            |> List.tryFind (fun p -> float(n-p)/2.0 |> sqrt |> isInteger)
            |> function
            | Some _ -> loop (n+1) primes
            | None -> n
        | _ -> loop (n+1) primes           
    loop 2 List.empty

findCurious ()