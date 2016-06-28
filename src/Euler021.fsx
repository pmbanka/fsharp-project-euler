open System
open System.Collections.Generic

let sumPropDiv (n:int) =
   let max = (float >> Math.Sqrt >> int) n
   let mutable sum = 1
   for i in 2 .. max do
       if n % i = 0 then
           sum <- sum + i
           let d = n / i
           if d <> i then
               sum <- sum + d
   sum

let isAmicable n =
    let sumFirst = sumPropDiv n
    let sumSecond = sumPropDiv sumFirst
    if sumFirst <> sumSecond && sumSecond = n then
        Some (sumSecond, sumFirst)
    else
        None

let isAmicableMem =
   let dict = new Dictionary<int,bool>()
   fun n ->
       match dict.TryGetValue(n) with
       | (true, v) -> v
       | _ ->
           match isAmicable n with
           | Some(a, b) -> 
                dict.Add(a, true)
                dict.Add(b, true)
                true
           | None -> 
                dict.Add(n, false)
                false
           
seq { 1 .. 10000 } |> Seq.where isAmicableMem |> Seq.sum