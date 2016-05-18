let euler2 n =
    let isEven x = x % 2 = 0
    let fib = Seq.unfold (fun (a, b) -> Some (a+b, (b, a+b))) (0, 1)
    fib 
    |> Seq.where isEven 
    |> Seq.takeWhile (fun x -> x < n) 
    |> Seq.sum
    
euler2 4000000