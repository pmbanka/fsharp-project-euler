open System

type divResult = { Div : int; Rem : int }

let getRecurringCycle (x : int) =
    let divRem (a : int) (b : int) =
        let div, rem = Math.DivRem (a, b)
        { Div = div; Rem = rem }

    let rec divide (a : int) (b : int) results =
        let dr = divRem a b
        if dr.Rem = 0 then
            None
        elif List.exists (fun x -> x.Rem = dr.Rem) results then
            Some (dr::results)
        else
            divide (10*dr.Rem) b (dr::results) 

    let extractCycle result =
        let first = (List.head result)
        result 
        |> Seq.skip 1 
        |> Seq.takeWhile (fun x -> x.Rem <> first.Rem)
        |> Seq.map (fun x -> x.Div)
        |> Seq.append [first.Div]
        |> Seq.rev
        |> Seq.map (fun x -> x.ToString ())
        |> String.concat ""  
 
    divide 1 x []
    |> Option.map extractCycle

{ 1 .. 999 }
|> Seq.maxBy (getRecurringCycle >> (function | Some x -> x | None -> "") >> String.length)