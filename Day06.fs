open System.IO
open System.Collections.Generic

let rec orbitCount x (d:Dictionary<_, _>) c = 
    match d.TryGetValue(x) with
    | true, value -> orbitCount value d (c+1)
    | _ -> c

let rec orbitList x (d:Dictionary<_, _>) (s:Set<_>) =
    let s = s.Add(x)
    match d.TryGetValue(x) with
    | true, value -> orbitList value d s
    | _ -> s

[<EntryPoint>]
let main argv =
    let orbits = new Dictionary<string, string>()
    File.ReadAllLines(argv.[0]) |> Array.iter(fun (x:string) -> 
        let v = x.Split(')')
        orbits.Add(v.[1], v.[0]))

    printfn "Part 1: %d" (orbits |> Seq.fold(fun acc (KeyValue(k,v)) -> orbitCount k orbits acc) 0)

    let YOU = orbitList "YOU" orbits Set.empty
    let SAN = orbitList "SAN" orbits Set.empty
    printfn "Part 2: %d" ((Set.union YOU SAN).Count - ((Set.intersect YOU SAN).Count) - 2) // YOU and SAN does not count
    0 // return an integer exit code
