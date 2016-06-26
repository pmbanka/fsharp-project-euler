open System

// http://stackoverflow.com/a/286821/1108916
let rec permutations list taken =
    seq {
        if List.length list = Set.count taken then 
            yield []
        else
            for l in list do
                if not (Set.contains l taken) then
                    for perm in permutations list (Set.add l taken) do
                        yield l::perm }

permutations [0;1;2;3;4;5;6;7;8;9] Set.empty
|> Seq.skip 999999
|> Seq.head