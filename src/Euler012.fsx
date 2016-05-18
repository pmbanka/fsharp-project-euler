open System

let triangleNums = Seq.unfold (fun (agr, idx) -> Some (agr+idx, (agr+idx, idx+1))) (0, 1)

triangleNums |> Seq.take 10 |> Seq.toArray

let numberOfDivisors (n:int) =
    let upperBound = Math.Sqrt (float n) |> Math.Floor |> int 
    let divisors = 
        seq { 1 .. upperBound }
        |> Seq.where (fun x -> n % x = 0)
        |> Seq.length
        |> (*) 2
    match upperBound*upperBound = n with
    | true -> divisors - 1
    | false -> divisors

#time
let euler12 =
    triangleNums
    |> Seq.find (fun n -> numberOfDivisors n > 500)
#time