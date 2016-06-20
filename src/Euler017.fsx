open System

let digitsLookup = 
    [ 
        '0', "zero";
        '1', "one";
        '2', "two";
        '3', "three";
        '4', "four";
        '5', "five";
        '6', "six";
        '7', "seven";
        '8', "eight";
        '9', "nine"     
    ] |> Map.ofList
            
let parseHundreds ch =
    match ch with
    | '0' -> ""
    | x -> digitsLookup.[ch] + " hundred"

let smallTensLookup =
    [
        '0', "ten";
        '1', "eleven";
        '2', "twelve";
        '3', "thirteen";
        '4', "fourteen";
        '5', "fifteen";
        '6', "sixteen";
        '7', "seventeen";
        '8', "eighteen";
        '9', "nineteen";
    ] |> Map.ofList
    
let bigTensLookup = 
    [
        '2', "twenty";
        '3', "thirty";
        '4', "forty";
        '5', "fifty";
        '6', "sixty";
        '7', "seventy";
        '8', "eighty";
        '9', "ninety";
    ] |> Map.ofList

let parseTens tensCh digitsCh =
    match (tensCh, digitsCh) with
    | ('0', '0') -> ""
    | ('0', d) -> digitsLookup.[d]
    | ('1', d) -> smallTensLookup.[d]
    | (t, '0') -> bigTensLookup.[t]
    | (t, d) -> bigTensLookup.[t] + "-" + digitsLookup.[d]

let parseTriplet (input:string) =
    if input.Length <> 3 then failwithf "Incorrect length %d" input.Length
    match input with
    | "000" -> ""
    | x ->
        let skipAnd = 
            input.[0] = '0' ||
            (input.[1] = '0' && input.[2] = '0')
        let conjunction =
            match skipAnd with
            | true -> ""
            | false -> " and "
        parseHundreds input.[0] + conjunction + parseTens input.[1] input.[2]

let toPaddedString n =
    let tmp = n.ToString ()
    let padding = 
        match tmp.Length % 3 with
        | 1 -> 2
        | 2 -> 1
        | _ -> 0
    tmp.PadLeft (tmp.Length + padding, '0')
    
let inline parseNumber n =
    if n = LanguagePrimitives.GenericZero then
        "zero"
    else
        let padded = toPaddedString n
        let tripletsNumber = padded.Length / 3
        let names = [""; "thousand"; "million"; "billion"; "trillion"; "quadrillion"; "quintillion" ]
        if names.Length < tripletsNumber then failwith "Input is too long"
        let triplets = 
            seq { 0 .. padded.Length / 3 - 1 }
            |> Seq.map (fun i -> padded.Substring (3*i, 3))
            |> Seq.map parseTriplet
            |> Seq.rev
        let merged =
            triplets
            |> Seq.zip names
            |> Seq.map (fun (name, triplet) -> if triplet <> "" then triplet + " " + name else "")
            |> Seq.rev
        let ret = String.Join (" ", merged)
        ret.Trim ()
     
parseNumber 99999999999999999999I
        
let eulerResult =
    seq { 1 .. 1000 }
    |> Seq.map parseNumber
    |> Seq.map (fun s -> s.Replace (" ", ""))
    |> Seq.map (fun s -> s.Replace ("-", ""))
    |> Seq.map (fun s -> s.Length)
    |> Seq.sum