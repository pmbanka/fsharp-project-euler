open System

let candidates = 
    seq {
        for i in 1 .. 999 do
            for j in 1 .. 999 do 
                yield i, j
    }
    
let check (a, b) =
    1000*a + 1000*b - a*b = 500000
    
let a, b = candidates |> Seq.find check
let c = 1000 - a - b
let abc = a * b * c