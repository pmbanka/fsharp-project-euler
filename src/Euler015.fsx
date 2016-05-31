open System
open System.Collections.Generic

let memoize f =
   let dict = new Dictionary<_,_>()
   fun n ->
       match dict.TryGetValue(n) with
       | (true, v) -> v
       | _ ->
           let temp = f(n)
           dict.Add(n, temp)
           temp

module NumericLiteralG = 
    let inline FromZero() = LanguagePrimitives.GenericZero
    let inline FromOne() = LanguagePrimitives.GenericOne
    let inline FromInt32 (n:int) =
        let one : ^a = FromOne()
        let zero : ^a = FromZero()
        let n_incr = if n > 0 then 1 else -1
        let g_incr = if n > 0 then one else (zero - one)
        let rec loop i g = 
            if i = n then g
            else loop (i + n_incr) (g + g_incr)
        loop 0 zero

let inline countRoutes (width:^a) (height:^a) : ^a =
   let maxX = width
   let maxY = height
   let rec count = memoize ( fun (x, y) ->
        match (x, y) with
        | (x, y) when x = maxX && y <> maxY -> count (maxX, (y + 1G))
        | (x, y) when x <> maxX && y = maxY -> count ((x + 1G), maxY)
        | (x, y) when x = maxX && y = maxY -> 1G
        | _ -> count ((x + 1G), y) + count (x, (y + 1G)) )
   count (0G, 0G)
    
#time
countRoutes 20I 20I
#time