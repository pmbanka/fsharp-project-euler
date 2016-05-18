let isEvenlyDivisible maxDivisible n  =
    let rec isEvenlyDivisibleImpl number divident =
        if divident = 1 then
            true
        else if number % divident = 0 then
            isEvenlyDivisibleImpl number (divident-1)
        else
            false
    isEvenlyDivisibleImpl n maxDivisible
       
isEvenlyDivisible 10 2520
  
let findSmallestEvenlyDivisible n =
    Seq.initInfinite (fun x -> 2 * x)
    |> Seq.skipWhile (fun x -> x < n)
    |> Seq.tryFind (isEvenlyDivisible n)

#time    
findSmallestEvenlyDivisible 20
#time
