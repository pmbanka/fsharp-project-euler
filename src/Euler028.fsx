open System

let solve arrSize =
    let repeat times value = seq { for i in 1 .. times do yield value }
    let increments = Seq.collect (repeat 4) (Seq.initInfinite (fun x -> (bigint x)+1I)) |> Seq.map ((*) 2I)
    let numberOfCircles = (arrSize - 1) / 2
    increments 
    |> Seq.take (numberOfCircles * 4)
    |> Seq.mapFold (fun state incr -> state + incr, state + incr) 1I
    |> fst 
    |> Seq.sum
    |> (+) 1I

solve 1001