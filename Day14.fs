open System.IO

let RemoveReaction x r =
    x |> Array.choose(fun el -> match r = snd (snd el) with | false -> Some el | _ -> None) 

let ReactionRequires reac s =
    match (fst reac) |> Array.tryFind(fun (_, x) -> x=s) with 
    | Some x -> true 
    | None -> false

let FindReactionFor reacs s =
    reacs |> Array.find(fun (_, (_, x)) -> x=s)

let FindNextReactant reac req =
    req |> Map.findKey(fun r _ -> not (reac |> Array.exists(fun reac -> ReactionRequires reac r)))

let GetNewReqValue (reqs:Map<_,_>) name amount =
    match reqs.TryFind(name) with
    | Some x -> x + amount
    | None -> amount

let Reagent (x:string) =
    let work = match x.[0] = ' ' with | true -> x.Substring(1) | _ -> x
    let s = work.Split(' ')
    (System.Int64.Parse s.[0], s.[1])

let ParseReaction (x:string) =
    let inOut = x.Replace("=", "").Split('>')
    let ins = inOut.[0].Split(',')
    let out = Reagent (inOut.[1])
    (Array.map(Reagent) ins, out)

let React r (req:Map<_,_>) = 
    let mutable requirements = req
    let times = (req.[snd(snd r)] + ((fst (snd r)) - 1L)) / fst (snd r)
    (fst r) |> Array.iter(fun (i, name) -> requirements <- requirements.Add(name, GetNewReqValue requirements name (times*i)))
    requirements

let trillion = 1000000000000L

let rec BinarySearch h l rc =
    let current = (h+l)/2L
    match h < l with 
    | true -> current 
    | _ -> let mutable requirements = Map.empty.Add("FUEL", current)
           rc |> Array.iter(fun r -> requirements <- React r requirements)
           match requirements.["ORE"] <= trillion with
           | true -> BinarySearch h (current + 1L) rc
           | false -> BinarySearch (current - 1L) l rc

[<EntryPoint>]
let main argv =
    let rawReactions = File.ReadAllLines(argv.[0])
    let mutable reactions = Array.map ParseReaction rawReactions
    let mutable requirements = Map.empty.Add("FUEL", 1L)
    let mutable reactionChain = Array.empty
    while reactions.Length > 0 do
        let reactant = FindNextReactant reactions requirements
        let reaction = FindReactionFor reactions reactant
        requirements <- React reaction requirements
        requirements <- requirements.Remove(snd (snd(reaction)))
        reactionChain <- Array.append reactionChain [|reaction|] 
        reactions <- RemoveReaction reactions reactant

    printfn "Part 1: %A" requirements.["ORE"]
    let lowBound = trillion / requirements.["ORE"]
    BinarySearch (lowBound*10L) lowBound reactionChain |> printfn "Part 2: %A fuel produced"
    0 // return an integer exit code
