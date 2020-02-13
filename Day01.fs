open System.IO;

let fuel x =
    match x >= 9 with 
    | true -> x / 3 - 2
    | false -> 0

let rec totalFuel x = 
    let f = fuel x
    match f > 0 with
    | true -> f + totalFuel(f)
    | false -> 0


[<EntryPoint>]
let main argv =
    let data = File.ReadLines(argv.[0]) |> Seq.map System.Int32.Parse
    
    printfn "Part 1: %d" (data |> Seq.sumBy fuel)
    printfn "Part 2: %d" (data |> Seq.sumBy totalFuel)

    0 // Returns exit code
