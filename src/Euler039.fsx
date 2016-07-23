open System

let candidtes p = seq {
    for i in 1 .. p / 2 do  
        for j in i .. p do
            yield i, j }

let getNumOfRightTriangles p =
    let test a b p = p*p - 2*p*a + 2*a*b - 2*p*b = 0
    candidtes p 
    |> Seq.where (fun (a, b) -> test a b p)
    |> Seq.length

seq { 3 .. 999 }
|> Seq.maxBy getNumOfRightTriangles