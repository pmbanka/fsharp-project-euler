open System

let getSumOfProperDivisors (n:int) =
   let max = (float >> Math.Sqrt >> int) n
   let mutable sum = 1
   for i in 2 .. max do
       if n % i =0 then
           sum <- sum + i
           let d = n / i
           if d <> i then
               sum <- sum + d
   sum

let isAbundantNumber (n:int) = 
   getSumOfProperDivisors n > n

let getAllabundantsArray max =
   seq { 1 .. max }
   |> Seq.where isAbundantNumber
   |> Seq.toArray

let magicNumber = 28123
let all = getAllabundantsArray magicNumber

let canBeWrittenAsSumOfAbundantNumbers (all:int[]) (n:int) =
   if n > magicNumber then
       true
   else
       let rec testSumPossibility (leftIdx:int) (rightIdx:int) =
           match leftIdx, rightIdx with
           | l, _ when l > all.Length-1 -> false
           | l, _ when all.[l] > n -> false
           | l, r when all.[l] + all.[r] = n -> true
           | l, r when all.[l] + all.[r] < n && r < all.Length-1 -> testSumPossibility l (r+1)
           | l, r -> testSumPossibility (l+1) (l+1)
       testSumPossibility 0 0

let result = 
   seq { 1 .. magicNumber }
   |> Seq.where (not << (canBeWrittenAsSumOfAbundantNumbers all))
   |> Seq.sum