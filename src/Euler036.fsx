open System

let isPalindromic s =
    let reversed = Seq.rev s
    s 
    |> Seq.zip reversed 
    |> Seq.forall (fun (a, b) -> a = b)

{ 1 .. 999999 }
|> Seq.where (fun n -> isPalindromic <| n.ToString () && isPalindromic <| Convert.ToString (n, 2))
|> Seq.sum