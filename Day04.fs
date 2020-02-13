let Monotonic x =
    x |> Seq.pairwise |> Seq.forall(fun x -> fst x <= snd x)

let HasPair x = 
    x |> Seq.pairwise |> Seq.exists(fun x -> fst x = snd x)

let HasStrictPair x =
    x |> Seq.countBy(fun x -> x) |> Seq.exists(fun x -> (snd x) = 2)

let rec Split x =
    match x < 10 with
    | true -> seq {x}
    | false -> seq { yield! (Split (x/10)); yield x%10 }  

[<EntryPoint>]
let main argv =
    let range = argv.[0].Split('-') |> Array.map (fun x -> System.Int32.Parse x)
    let sequence = seq { for i in range.[0] .. range.[1] do Split i} |> Seq.filter(fun x -> Monotonic x)
    sequence |> Seq.filter( fun x -> HasPair x) |> Seq.length |> printfn "Part 1 %d" 
    sequence |> Seq.filter( fun x -> HasStrictPair x) |> Seq.length |> printfn "Part 2 %d"

    0 // return an integer exit code
