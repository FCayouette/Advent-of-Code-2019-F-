open System.IO;

let GetFirst (x : int[] ) y =
    match ((x.[y]/100)%10) with
        | 0 -> x.[x.[y+1]]
        | _ -> x.[y+1]

let GetSecond (x : int[]) y =
    match ((x.[y]/1000)%10) with
        | 0 -> x.[x.[y+2]]
        | _ -> x.[y+2]

let rec IntcodeAdd (x : int[]) y input = 
    x.[x.[y+3]] <- (GetFirst x y) + (GetSecond x y)
    IntcodeProcess x (y+4) input

and IntcodeMul (x : int[]) y input = 
    x.[x.[y+3]] <- (GetFirst x y) * (GetSecond x y)
    IntcodeProcess x (y+4) input

and IntcodeGet (x : int[]) y (input:List<int>) =
    x.[x.[y+1]] <- input.Head
    IntcodeProcess x (y+2) input.Tail

and IntcodePrint (x : int[]) y input =
    printfn "%d" (GetFirst x y) 
    IntcodeProcess x (y+2) input

and IntcodeJumpIfTrue (x : int[]) y input =
    match GetFirst x y with
        | 0 -> IntcodeProcess x (y+3) input
        | _ -> IntcodeProcess x (GetSecond x y) input

and IntcodeJumpIfFalse (x : int[]) y input =
    match GetFirst x y with
        | 0 -> IntcodeProcess x (GetSecond x y) input
        | _ -> IntcodeProcess x (y+3) input

and IntcodeLessThan (x : int[]) y input =
    match (GetFirst x y) < (GetSecond x y) with
        | true -> x.[x.[y+3]] <- 1
        | false -> x.[x.[y+3]] <- 0
    IntcodeProcess x (y+4) input

and IntcodeEquals (x : int[]) y input =
    match (GetFirst x y) = (GetSecond x y) with
        | true -> x.[x.[y+3]] <- 1
        | false -> x.[x.[y+3]] <- 0
    IntcodeProcess x (y + 4) input

and IntcodeProcess (x : int[]) y input =
    match (x.[y] % 100) with
        | 1 -> IntcodeAdd x y input
        | 2 -> IntcodeMul x y input
        | 3 -> IntcodeGet x y input
        | 4 -> IntcodePrint x y input
        | 5 -> IntcodeJumpIfTrue x y input
        | 6 -> IntcodeJumpIfFalse x y input
        | 7 -> IntcodeLessThan x y input
        | 8 -> IntcodeEquals x y input
        | _ -> x.[0]


let FirstPart x =
    let input = [1]
    IntcodeProcess (x |> Seq.toArray) 0 input

let SecondPart x =
    let input = [5]
    IntcodeProcess (x |> Seq.toArray) 0 input

[<EntryPoint>]
let main argv =
    let program = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int32.Parse 
    FirstPart program |> ignore
    SecondPart program |> ignore
    0 // return an integer exit code
