open System.IO;

let rec IntcodeAdd (x : int[]) y = 
    x.[x.[y+3]] <- x.[x.[y+1]] + x.[x.[y+2]]
    let y = y + 4
    IntcodeProcess x y

and IntcodeMul (x : int[]) y = 
    x.[x.[y+3]] <- x.[x.[y+1]] * x.[x.[y+2]]
    let y = y + 4
    IntcodeProcess x y

and IntcodeProcess (x : int[]) y =
    match x.[y] with
        | 1 -> IntcodeAdd x y
        | 2 -> IntcodeMul x y
        | _ -> x.[0]
    
let firstPart x =
    let program = x |> Array.ofSeq
    program.[1] <- 12
    program.[2] <- 2
    IntcodeProcess program 0

let ComputeRun x noun verb =
    let program = x |> Array.ofSeq
    program.[1] <- noun
    program.[2] <- verb
    (IntcodeProcess program 0, noun, verb)

let rec secondPart x n v previous =
    let (a, b, c) = (ComputeRun x n v)
    match a < 19690720 with
    | true -> secondPart x (n+1) v a
    | false -> match c = 0 with
        | true -> (a, n-1, 19690720 - previous)
        | false -> (a,b,c)
[<EntryPoint>]
let main argv =
    let backupProgram = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int32.Parse
    
    printfn "%d" (firstPart backupProgram)

    let (_, b, c) = secondPart backupProgram 0 0 0
    printfn "%d" (b*100 + c)
    0 // return an integer exit code
