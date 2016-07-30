open System
open System.IO

let getAlphabetPosition (ch:char) =
    if not <| Char.IsLetter ch then failwithf "%A is not a letter" ch
    let asciiCode = Convert.ToInt32 ch
    if Char.IsUpper ch then
        asciiCode - 65 + 1
    else
        asciiCode - 97 + 1

let getWordCount str =
    str
    |> Seq.map getAlphabetPosition
    |> Seq.sum

let triangleNums = Seq.initInfinite (fun x -> (x+1)*(x+2)/2)
let triangleNumsSet = triangleNums |> Seq.take 100 |> Set.ofSeq

let path = "Euler042.txt"
let total = 
   File.ReadAllText path 
   |> fun s -> s.Split ','
   |> Seq.map (fun x -> x.Replace ("\"", ""))
   |> Seq.map getWordCount
   |> Seq.where (fun x -> Set.contains x triangleNumsSet)
   |> Seq.length