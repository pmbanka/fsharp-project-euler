open System

module List = 
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let isCurious l =
    let divisors = [1; 2; 3; 5; 7; 11; 13; 17]
    let rec loop list divs =
        match list with
        | a::b::c::_ ->
            let num = a*100 + b*10 + c
            printfn "Checking %d" num
            if num % (List.head divs) = 0 then
                loop (List.tail list) (List.tail divs)
            else
                false
        | _ -> true
    loop l divisors

let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0]
l |> isCurious
match l with
| a::b::c::tail -> true
| _ -> false




let input = [0 .. 9]
List.permutations input
|> Seq.length