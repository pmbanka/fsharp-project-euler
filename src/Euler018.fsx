open System

type tree =
    | Leaf
    | Node of int * tree * tree

let loadTreeFromFile path =
    let lines = System.IO.File.ReadAllLines path
    let array = lines |> Array.map (fun s -> s.Split [|' '|] |> Array.map Convert.ToInt32)
    let getElement x y = array.[y].[x]
    let isBottomOfTree height = height = array.Length
    let rec createNode x y =
        if isBottomOfTree y then
            Leaf
        else
            Node (getElement x y, createNode x (y+1), createNode (x+1) (y+1))
    createNode 0 0
    
let getMaxPath tree =
    let rec loop = function
        | Leaf -> 0
        | Node(value, tree1, tree2) ->
            value + Math.Max((loop tree1), (loop tree2))
    loop tree

getMaxPath (loadTreeFromFile "src/Euler018.txt")