open System

let countCoins target (coins:int[]) =
    let rec count target coinIdx =
        match target, coinIdx with
        | t, idx when t < 0 || idx < 0 -> 0
        | 0, _ -> 1
        | _ -> (count target (coinIdx-1)) + (count (target-coins.[coinIdx]) coinIdx)
    count target (coins.Length-1)

countCoins 200 [|1; 2; 5; 10; 20; 50; 100; 200|]