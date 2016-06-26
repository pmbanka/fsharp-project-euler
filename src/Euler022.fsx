open System
open System.IO

let toAlphabetValue (s:string) =
   s 
   |> Seq.map Char.ToLower
   |> Seq.map (fun c -> int c - int 'a' + 1)
   |> Seq.sum
   |> bigint

let path = "src/Euler022.txt"
let total = 
   File.ReadAllText path 
   |> fun s -> s.Split ','
   |> Seq.map (fun x -> x.Replace ("\"", ""))
   |> Seq.sort
   |> Seq.map toAlphabetValue
   |> Seq.mapi (fun i elem -> bigint (i+1) * elem)
   |> Seq.sum