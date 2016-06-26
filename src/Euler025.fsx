open System

let fib = Seq.unfold (fun (a, b) -> Some (a+b, (a+b, a))) (0I, 1I)

let numOfDigits (n:bigint) = 
    n.ToString () |> String.length

fib 
|> Seq.map numOfDigits 
|> Seq.findIndex (fun x -> x > 999) 
|> (+) 1