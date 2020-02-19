open System.IO

let origin = [|0;0;0|]

let speedDelta x y = 
        Array.map2(fun x y -> match y - x with | 0 -> 0 | d -> d/(abs d)) (fst x) (fst y)
    
let applySpeedDelta x =
    (Array.map2(+) (fst x) (snd x), snd x)

let computeSpeedDelta body x =
     x |> Array.map(fun x -> speedDelta body x) |> Array.reduce(fun a b -> Array.map2(+) a b)

let computeSpeedDeltas x =
    x |> Array.map(fun p -> computeSpeedDelta p x) |> Array.map2(fun (_,x) y -> Array.map2(+) x y) x |>
         Array.map2(fun x y -> (fst x, y)) x

let energy x = 
    x |> Array.sumBy(fun (x, y) -> (Array.sumBy(fun x -> abs x) x) * (Array.sumBy(fun y -> abs y) y))

let extractPos (x:string) =
    (x.Split(',') |> Array.map(fun x -> x.Substring(x.IndexOf('=')+1) |> System.Int32.Parse), origin)

let removeTrailing (x:string) =
    x.Substring(0, x.LastIndexOf('>'))

let tick = computeSpeedDeltas >> Array.map(applySpeedDelta)

let rec simulate count d =
    match count > 0 with
    | false -> d
    | true -> simulate (count - 1) (tick d)

let rec GCD a b =
    let c = a % b
    match c with
    | 0L -> b
    | _ -> GCD b c

let LCM x y =
    x * y / (match x < y with | true -> (GCD y x) | _ -> (GCD x y))

let shouldGoOn x =
    match x |> Array.tryFind(fun x -> x = 0L) with
    | None -> false
    | _ -> true

let findPeriod x =
    let getPos = Array.map(fun (x,y) -> x)
    let startPos = getPos x
    let zeros = [|0;0;0|]
    let mutable periods = [|0L;0L;0L|]
    let mutable data = x
    let mutable iter = 1L
    while shouldGoOn periods do
        data <- tick data
        iter <- iter+1L
        let matches = Array.fold2(fun a x y -> Array.map3(fun a x y -> match x=y with | true -> a+1 | _ -> a) a x y) zeros startPos (getPos data)
        match Array.tryFindIndex(fun x -> startPos.Length = x) matches with
            | Some i -> match periods.[i] = 0L with
                        | true -> periods.[i] <- iter
                        | _ -> ()
            | None -> ()
    periods |> Seq.reduce(LCM)

[<EntryPoint>]
let main argv =
    let data = File.ReadAllLines(argv.[0]) |> Array.map removeTrailing |> Array.map extractPos
    data |> simulate 1000 |> energy |> printfn "Part 1: %A" 
    data |> findPeriod |> printfn "Part 2: %A"

    0 // return an integer exit code
