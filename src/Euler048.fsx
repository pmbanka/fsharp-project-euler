open System

module String = 
    let ofSeq (seq: seq<char>) = 
        seq |> (System.Text.StringBuilder () |> Seq.fold (fun sb c -> sb.Append(c))) |> string

let huuuge = 
    seq { 1 .. 1000 }
    |> Seq.map (fun x -> bigint.Pow (bigint x, x))
    |> Seq.sum

let str = huuuge.ToString ()
str |> Seq.skip (str.Length-10) |> String.ofSeq