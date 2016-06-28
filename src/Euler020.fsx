open System
open System.Numerics

let factorial (n:bigint) =
   let mutable acc = 1I
   for i in 2I .. n do
       acc <- acc * i
   acc
   
let digits (n:bigint) =
   n.ToString ()
   |> Seq.map Char.GetNumericValue
   |> Seq.map Convert.ToInt32
   
digits (factorial 100I) |> Seq.sum