let euler1 n =
    let threes = seq { 3 .. 3 .. n }
    let fives = seq { 5 .. 5 .. n }
    threes 
    |> Seq.append fives 
    |> Seq.distinct 
    |> Seq.sum
    
euler1 999