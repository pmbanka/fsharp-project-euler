open System

module List = 
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec insertions x = function
        | [] -> [[x]]
        | (y::ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | [] -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let isCurious l =
    let divisors = [1; 2; 3; 5; 7; 11; 13; 17]
    let rec loop list divs =
        match list with
        | a::b::c::_ ->
            let num = a*100 + b*10 + c
            if num % (List.head divs) = 0 then
                loop (List.tail list) (List.tail divs)
            else
                false
        | _ -> true
    loop l divisors

let toInt64 l =
    List.rev l 
    |> Seq.zip (Seq.initInfinite (fun x -> pown 10 x))
    |> Seq.map (fun (t1, t2) -> int64 t1 * int64 t2)
    |> Seq.sum

List.permutations [0..9]
|> Seq.where isCurious
|> Seq.map toInt64
|> Seq.sum