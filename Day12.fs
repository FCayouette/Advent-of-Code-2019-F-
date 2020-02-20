open System.IO

let speedDelta (x,_) (y,_) = 
    Array.map2(fun x y -> match y - x with | 0 -> 0 | d -> d/(abs d)) x y
    
let applySpeedDelta (x,y) =
    (Array.map2(+) x y, y)

let computeSpeedDelta body x =
     x |> Array.map(fun x -> speedDelta body x) |> Array.reduce(fun a b -> Array.map2(+) a b)

let computeSpeedDeltas x =
    x |> Array.map(fun p -> computeSpeedDelta p x) |> Array.map2(fun (_,x) y -> Array.map2(+) x y) x |>
         Array.map2(fun (x,_) y -> (x, y)) x

let tick = computeSpeedDeltas >> Array.map(applySpeedDelta)

let energy x = 
    x |> Array.sumBy(fun (x, y) -> Array.sumBy abs x * Array.sumBy abs y)

let extractPos (x:string) =
    x.Split(',') |> Array.map(fun x -> x.Substring(x.IndexOf('=')+1) |> System.Int32.Parse)

let removeTrailing (x:string) =
    x.Substring(0, x.LastIndexOf('>'))

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

let findPeriod x zeros =
    let getPos = Array.map(fun (x,_) -> x)
    let startPos = getPos x
    let mutable periods = zeros |> Array.map(fun x -> (int64)x) 
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
    periods |> Seq.reduce LCM

[<EntryPoint>]
let main argv =
    let posOnly = File.ReadAllLines(argv.[0]) |> Array.map removeTrailing |> Array.map extractPos
    let origin = [|for i in 1 .. posOnly.[0].Length -> 0|]
    let data = posOnly |> Array.map(fun x -> (x, origin))
    data |> simulate 1000 |> energy |> printfn "Part 1: %A" 
    findPeriod data origin |> printfn "Part 2: %A"
    0 // return an integer exit code
