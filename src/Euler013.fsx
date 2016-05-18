open System

let path = "src/Euler013.txt"
let lines = System.IO.File.ReadAllLines path

let getBigSum (array : string[]) =
    let charToInt (c:char) = (int c) - (int '0')
    let rec getPositionSum (idx:int) (carry:int) (result:string) =
        match idx with
        | -1 -> 
            match carry with 
            | 0 -> result
            | x -> Convert.ToString carry + result
        | x -> 
            let columnSum = 
                array
                |> Seq.map (fun x -> x.[idx])
                |> Seq.map charToInt
                |> Seq.sum
                |> (+) carry
            let newCarry, addToResult =
                match columnSum with
                | s when s < 9 -> (0, s)
                | s -> Math.DivRem (s, 10)
            let newResult = Convert.ToString addToResult + result
            getPositionSum (idx-1) newCarry newResult
    getPositionSum (array.[0].Length-1) 0 ""

let result = getBigSum lines
let firstPart = result.Substring (0, Math.Min(result.Length, 10))
