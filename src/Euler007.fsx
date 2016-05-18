open System

let isPrime primesBefore number  =
    if number % 2 = 0 then
        false
    else
        let upperBound = int (Math.Sqrt (float number)) + 1
        let divisibleBy = 
            primesBefore
            |> Seq.takeWhile (fun prime -> prime <= upperBound)
            |> Seq.tryFind (fun prime -> number % prime = 0)
        match divisibleBy with
        | Some _ -> false
        | None -> true
        
let findPrime primeIdx =
    let allPrimes = Array.zeroCreate primeIdx
    allPrimes.[0] <- 2
    let rec findPrimeRec lastPrimeIdx currentNumber =
        if lastPrimeIdx = allPrimes.Length - 1 then
            allPrimes |> Array.last // found last
        else if isPrime (allPrimes |> Seq.take (lastPrimeIdx + 1)) currentNumber then
            allPrimes.[lastPrimeIdx + 1] <- currentNumber
            findPrimeRec (lastPrimeIdx + 1) (currentNumber + 1)
        else
            findPrimeRec lastPrimeIdx (currentNumber + 1)
    findPrimeRec 0 2
            
findPrime 10001