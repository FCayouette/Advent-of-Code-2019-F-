open System.IO

let width = 25
let size = width * 6

let rec splitImage s (x:string) = 
    match x.Length > s with
    | true -> (x.Substring(0, s))::(splitImage s (x.Substring(s)))
    | _ -> [x]

let count (x:string) =
    let histogram = Map.empty.Add('0', 0).Add('1', 0).Add('2', 0)
    x |> Seq.fold (fun hist c -> Map.add c (hist.[c] + 1) hist) histogram

let rec decode top next =
    match next with
    | [] -> top
    | x::xs -> decode (Seq.map2 (fun x y -> match x = '2' with | true -> y | false -> x) top x) xs 

[<EntryPoint>]
let main argv =
    let image = File.ReadAllText(argv.[0]) |> splitImage size
    let lowest0 = image |> List.map count |> List.sortBy (fun x -> x.['0']) |> List.head
    printfn "Part 1: %d" (lowest0.['1'] * lowest0.['2'])

    let result = decode image.Head image.Tail |> Seq.map (fun x -> match x = '0' with | true -> " " | _ -> "#") |> String.concat "" |> splitImage width
    result |> List.iter (fun x -> printfn "%A" x)
    0 // return an integer exit code
