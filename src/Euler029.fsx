open System

let getAllPairs fromVal toVal =
   seq {
       for i in fromVal .. toVal do
           for j in fromVal .. toVal do
               yield i, j }

getAllPairs 2 100
|> Seq.map (fun (a, b) -> bigint.Pow (bigint a, b))
|> Seq.distinct
|> Seq.length