open System

let getNumOfRightTriangles p =
    let test p a = (2*p*a - p*p) % (2*a - 2*p) = 0
    { 1 .. p/2 }
    |> Seq.where (test p)
    |> Seq.length

seq { 3 .. 999 }
|> Seq.maxBy getNumOfRightTriangles