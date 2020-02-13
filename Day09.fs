open System.IO;

type ProgramState(programIn : int64[], x, relBase, inputs) =
    member this.program = programIn
    member this.pc = x
    member this.relative = relBase
    member this.input = inputs

let Mode (x : int64[]) y offset = 
    match offset with
    | 1 -> (x.[y] / 100L) % 10L
    | 2 -> (x.[y] / 1000L ) % 10L
    | _ -> (x.[y] / 10000L ) % 10L

let Index x y r i =
    match (Mode x y i) with
    | 0L -> (int)x.[y+i]
    | 1L -> y + i
    | _ -> (int)(x.[y+i] + r)

let Get (x : int64[]) y r i =
    let index = Index x y r i
    match index < x.Length with
    | true -> x.[index]
    | false -> 0L

let Write (x : int64[]) y r i v =
    let index = Index x y r i
    match index < x.Length with
        | true -> x.[index] <- v
                  x
        | false -> let newProgram = Array.zeroCreate (index+1)
                   newProgram.[index] <- v
                   Array.blit x 0 newProgram 0 x.Length
                   newProgram

let IntcodeAdd (x : int64[]) y r input = 
    (0, ProgramState((Write x y r 3 ((Get x y r 1) + (Get x y r 2))), (y+4), r, input))

let IntcodeMul (x : int64[]) y r input = 
    (0, ProgramState((Write x y r 3 ((Get x y r 1) * (Get x y r 2))), (y+4), r, input))

let IntcodeGet (x : int64[]) y r (input:List<int64>) =
    match input with
    | [] -> (1, ProgramState(x, y, r, List.Empty))
    | h::t -> (0, ProgramState((Write x y r 1 h), (y+2), r, t))

let IntcodePrint (x : int64[]) y r input =
    printfn "%A" (Get x y r 1)
    (0, ProgramState(x, (y+2), r, input))

let IntcodeJumpIfTrue (x : int64[]) y r input =
    match Get x y r 1 with
        | 0L -> (0, ProgramState(x, (y+3), r, input))
        | _ -> (0, ProgramState(x, ((int)(Get x y r 2)), r, input))

let IntcodeJumpIfFalse (x : int64[]) y r input =
    match Get x y r 1 with
        | 0L -> (0, ProgramState(x, ((int)(Get x y r 2)), r, input))
        | _ -> (0, ProgramState(x, (y+3), r, input))

let IntcodeLessThan (x : int64[]) y r input =
    let v = match (Get x y r 1) < (Get x y r 2) with
                | true -> 1L
                | false -> 0L
    (0, ProgramState((Write x y r 3 v), (y+4), r, input))

let IntcodeEquals (x : int64[]) y r input =
    let v = match (Get x y r 1) = (Get x y r 2) with
                | true -> 1L
                | false -> 0L
    (0, ProgramState((Write x y r 3 v), (y + 4), r, input))

let IntcodeRelBase (x : int64[]) y r input =
    (0, ProgramState(x, (y+2), (r + Get x y r 1), input))

let IntcodeProcess (x : int64[]) y r input =
    match (x.[y] % 100L) with
        | 1L -> IntcodeAdd x y r input
        | 2L -> IntcodeMul x y r input
        | 3L -> IntcodeGet x y r input
        | 4L -> IntcodePrint x y r input
        | 5L -> IntcodeJumpIfTrue x y r input
        | 6L -> IntcodeJumpIfFalse x y r input
        | 7L -> IntcodeLessThan x y r input
        | 8L -> IntcodeEquals x y r input
        | 9L -> IntcodeRelBase x y r input
        | _ -> (99, ProgramState(x, y+1, r, input))

let RunProgram p input =
    let program = p |> Seq.toArray
    let mutable runResult = IntcodeProcess program 0 0L input
    while fst runResult <> 99 do
        let state = snd runResult
        runResult <- IntcodeProcess state.program state.pc state.relative state.input

[<EntryPoint>]
let main argv =
    let program = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int64.Parse
    RunProgram program [1L]
    RunProgram program [2L]
    0 // return an integer exit code
