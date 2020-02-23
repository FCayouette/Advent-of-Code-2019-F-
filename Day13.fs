open System.IO;

type ProgramState(programIn : int64[], x, relBase, inputs, output) =
    member this.program = programIn
    member this.pc = x
    member this.relative = relBase
    member this.input = inputs
    member this.output = output

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
    (0, ProgramState((Write x y r 3 ((Get x y r 1) + (Get x y r 2))), (y+4), r, input, 0L))

let IntcodeMul (x : int64[]) y r input = 
    (0, ProgramState((Write x y r 3 ((Get x y r 1) * (Get x y r 2))), (y+4), r, input, 0L))

let IntcodeGet (x : int64[]) y r (input:List<int64>) =
    match input with
    | [] -> (1, ProgramState(x, y, r, List.Empty, 0L))
    | h::t -> (0, ProgramState((Write x y r 1 h), (y+2), r, t, 0L))

let IntcodePrint (x : int64[]) y r input =
    (2, ProgramState(x, (y+2), r, input, (Get x y r 1)))

let IntcodeJumpIfTrue (x : int64[]) y r input =
    match Get x y r 1 with
        | 0L -> (0, ProgramState(x, (y+3), r, input, 0L))
        | _ -> (0, ProgramState(x, ((int)(Get x y r 2)), r, input, 0L))

let IntcodeJumpIfFalse (x : int64[]) y r input =
    match Get x y r 1 with
        | 0L -> (0, ProgramState(x, ((int)(Get x y r 2)), r, input, 0L))
        | _ -> (0, ProgramState(x, (y+3), r, input, 0L))

let IntcodeLessThan (x : int64[]) y r input =
    let v = match (Get x y r 1) < (Get x y r 2) with
                | true -> 1L
                | false -> 0L
    (0, ProgramState((Write x y r 3 v), (y+4), r, input, 0L))

let IntcodeEquals (x : int64[]) y r input =
    let v = match (Get x y r 1) = (Get x y r 2) with
                | true -> 1L
                | false -> 0L
    (0, ProgramState((Write x y r 3 v), (y + 4), r, input, 0L))

let IntcodeRelBase (x : int64[]) y r input =
    (0, ProgramState(x, (y+2), (r + Get x y r 1), input, 0L))

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
        | _ -> (99, ProgramState(x, y+1, r, input, 0L))

let RunUntilSpecialCode (p:ProgramState) =
    let mutable runResult = IntcodeProcess p.program p.pc p.relative p.input
    while fst runResult = 0 do
        let state = snd runResult
        runResult <- IntcodeProcess state.program state.pc state.relative state.input
    runResult

let Part1 p =
    let initialState = ProgramState(p |> Seq.toArray, 0, 0L, [], 0L)
    let mutable runResult = RunUntilSpecialCode initialState
    let mutable outputs = []
    let mutable map = Map.empty

    while fst runResult <> 99 do
        let state = snd runResult
        match fst runResult with
        | 2 -> match outputs.Length with
               | 2 -> let y = outputs.Head
                      let x = outputs.Tail.Head
                      map <- map.Add((x,y), state.output)
                      outputs <- [] // Prepare for next run
               | _ -> outputs <- state.output::outputs
        | _ -> runResult <- (0, ProgramState(state.program, state.pc, state.relative, [], 0L))
        runResult <- RunUntilSpecialCode (snd runResult)
    // Count the number of blocks in the output: 
    map 
  
let findInput ball paddle =
    match ball - paddle with
        | 0L -> 0L
        | x -> x / (abs x)
  
let Part2 pr screen =
    let program = pr |> Seq.toArray
    program.[0] <- 2L
    let mutable ball = -1L
    let mutable paddle = -1L
    screen |> Map.iter(fun (x,_) t -> match t with | 4L -> ball <- x | 3L -> paddle <- x | _ -> ())
    let initialState = ProgramState(program, 0, 0L, [findInput ball paddle], 0L)
    let mutable runResult = RunUntilSpecialCode initialState
    let mutable outputs = []
    let mutable lastScore = 0L

    while fst runResult <> 99 do
        let state = snd runResult
        match fst runResult with
        | 2 -> match outputs.Length < 2 with
               | true -> outputs <- state.output::outputs
               | _ -> let x = outputs.Tail.Head
                      match x = -1L with
                        | true -> lastScore <- state.output
                        | _ ->  match state.output with
                                | 4L -> ball <- x
                                | 3L -> paddle <- x
                                | _ -> ()
                      outputs <- [] // Prepare for next run

        | _ -> runResult <- (0, ProgramState(state.program, state.pc, state.relative, [findInput ball paddle], 0L))
        runResult <- RunUntilSpecialCode (snd runResult)
    (int)lastScore

[<EntryPoint>]
let main argv =
    let program = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int64.Parse
    let init = program |> Part1
    init |> Map.fold(fun c _ v -> match v = 2L with | true -> c+1 | _ -> c) 0 |> printfn "Part 1: %A"
    (Part2 program init) |> printfn "Part 2: %A"
    0 // return an integer exit code
