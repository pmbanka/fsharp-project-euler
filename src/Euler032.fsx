open System

type Triple = { multiplicand : int; multiplier : int; product : int }

let compareSet = Set.ofList ['1' .. '9']
let isPandigital triple =
   let n = sprintf "%d%d%d" triple.multiplicand triple.multiplier triple.product
   if n.Length <> 9 then 
       false
   else
       let s = n |> Set.ofSeq
       Set.isSubset compareSet s

let getPandigitals max = seq { 
   for i in 1 .. max do
       yield! seq { 1 .. max }
       |> Seq.map (fun j -> { multiplicand = i; multiplier = j; product = i*j })
       |> Seq.where isPandigital }

#time "on"
getPandigitals 9876
|> Seq.distinctBy (fun x -> x.product)
|> Seq.sumBy (fun x -> x.product)
#time "off"