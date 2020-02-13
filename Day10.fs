open System.IO

let rec GDCimpl a b =
    let c = a % b
    match c with
    | 0 -> b
    | _ -> GDCimpl b c

let GDC a b =
    let aa = abs a
    let ab = abs b
    match aa > ab with
    | true -> GDCimpl aa ab
    | _ -> GDCimpl ab aa


let Delta x y =
    let d = (fst y - fst x, snd y - snd x)
    match d with
    | (0, 0) -> d
    | (0, x) -> (0, x / (abs x))
    | (x, 0) -> (x / (abs x), 0)
    | (x, y) -> let gdc = GDC x y
                (x/gdc, y/gdc)

let ComputeVisible cur ast = // returns (visibleCount, (cur)) where cur is the observation position
   (ast |> Seq.map(fun x -> Delta cur x) |> Seq.distinct |> Seq.filter(fun x -> x <> (0,0)) |> Seq.length, cur)
    
let ProcessLine (x:string) y =
   x |> Seq.mapi(fun i x -> match x with | '#' -> i | _ -> -1) |> Seq.filter(fun x -> x <> -1) |> Seq.map(fun x -> (x, y))

let ComputeAngle x b =
    let dx = fst x - fst b
    let dy = snd x - snd b
    let angle = atan2 (-(float)dx) ((float)dy)
    (angle, (dx, dy)) // Return angle to base and the delta location

[<EntryPoint>]
let main argv =
    // Create a list of (x, y) tuple for each asteroid
    let asteroids = File.ReadAllLines(argv.[0]) |> Seq.mapi(fun i x -> ProcessLine x i) |> Seq.concat

    let visible = asteroids |> Seq.map(fun x -> ComputeVisible x asteroids) |> Seq.sortDescending
    printfn "Part 1: %A" (fst (Seq.head visible))
    let monitor = snd (Seq.head visible)
    let zapped = snd (asteroids |> Seq.map(fun x -> ComputeAngle x monitor) |> Seq.sortBy(fun (x,(y,z)) -> (abs y) + (abs z)) 
                            |> Seq.distinctBy(fun (x,y) -> x) |> Seq.sortBy(fun (x,y) -> x) |> Seq.skip 199 |> Seq.head)
    
    printfn "Part 2: %A" ((fst zapped + fst monitor) * 100 + snd zapped + snd monitor)
    0 // return an integer exit code
