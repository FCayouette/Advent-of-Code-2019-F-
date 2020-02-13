open System.IO

let ToDelta c =
    match c with
    | 'R' -> (0,1)
    | 'L' -> (0, -1)
    | 'U' -> (-1, 0)
    | _ -> (1, 0)

let ToDir (s : string) =
    (ToDelta s.[0], System.Int32.Parse(s.[1..]))

let GenerateWirePath (wire:string[]) =
    let deltas = Array.collect(fun (token:string) -> 
                    let (a,b) = (ToDir token)
                    [| for i in 1 .. b -> a |]) wire
    deltas |> Array.scan(fun runningSum x -> ((fst runningSum + fst x), (snd runningSum + snd x))) (0,0) |> Array.skip 1

[<EntryPoint>]
let main argv =
    let wires = (File.ReadAllText(argv.[0]) |> String.map(fun c -> match c with  | ',' -> ' ' | _ -> c)).Split('\n')
    let expanded = wires |> Array.map (fun (x:string) -> GenerateWirePath (x.Split(' ')))

    let collisions = Set.intersect (set expanded.[0]) (set expanded.[1])
    
    let dists = collisions |> Set.map(fun x -> abs (fst x) + abs (snd x))

    printfn "Part 1: %d" dists.MinimumElement 

    let lengths = collisions |> Set.map(fun x -> (Array.findIndex( fun elem -> x = elem ) expanded.[0]) + (Array.findIndex(fun elem -> x = elem ) expanded.[1]) + 2 )
    printfn "Part 2: %d" lengths.MinimumElement
    0 // return an integer exit code
