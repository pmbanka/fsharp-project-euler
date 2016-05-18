let rec toDigitsRev n = 
    seq {
        let quotient, remainder = System.Math.DivRem (n, 10)
        yield remainder
        if quotient <> 0 then yield! toDigitsRev quotient
    }

let isPalindromic n =
    let reversed = n |> toDigitsRev |> Seq.toArray
    let normal = reversed |> Array.rev
    normal 
    |> Seq.zip reversed 
    |> Seq.forall (fun (a, b) -> a = b)
    
let crossProduct s1 s2 =
    seq {
        for e1 in s1 do 
            for e2 in s2 do 
                yield e1, e2 }

let euler4 =
    let numbers = seq { 100 .. 999 }
    crossProduct numbers numbers 
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.distinct
    |> Seq.where isPalindromic
    |> Seq.max