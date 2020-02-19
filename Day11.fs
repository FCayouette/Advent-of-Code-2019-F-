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


type RobotState(px, py, d) =
    member this.posX = px
    member this.posY = py
    member this.dir = (int)d
    member this.move(dd) =
        let newD = match dd with | 0L-> (this.dir + 3) % 4 | _ -> (this.dir + 1) % 4
        let newX = this.posX + (match newD with | 1 -> 1 | 3 -> -1 | _ -> 0)
        let newY = this.posY + (match newD with | 0 -> -1 | 2 -> 1 | _ -> 0)
        RobotState(newX, newY, newD)

let GetColor (x:Map<_,_>) pos =
    match x.TryFind pos with
    | Some p -> p
    | None -> 0L

let RunUntilSpecialCode (p:ProgramState) =
    let mutable runResult = IntcodeProcess p.program p.pc p.relative p.input
    while fst runResult = 0 do
        let state = snd runResult
        runResult <- IntcodeProcess state.program state.pc state.relative state.input
    runResult

let RunProgram p startColor =
    let initialState = ProgramState(p |> Seq.toArray, 0, 0L, [startColor], 0L)
    let mutable runResult = RunUntilSpecialCode initialState
    let mutable outputs = []
    let mutable robot = RobotState(0, 0, 0)
    let mutable map = Map.empty.Add((0,0), startColor)

    while fst runResult <> 99 do
        let state = snd runResult
        match fst runResult with
        | 2 -> match outputs.Length with
               | 1 -> map <- map.Add((robot.posY, robot.posX), outputs.Head)
                      robot <- robot.move(state.output)
                      outputs <- []
               | _ -> outputs <- [state.output]
        | _ -> runResult <- (0, ProgramState(state.program, state.pc, state.relative, [(GetColor map (robot.posY, robot.posX))], 0L))
        runResult <- RunUntilSpecialCode (snd runResult)
    map

let ConvertToChar (map:Map<_,_>) p =
    match map.TryFind(p) with 
    | Some x -> match x with 
                | 1L -> "#"
                | _ -> " "
    | None -> " "
   
[<EntryPoint>]
let main argv =
    let program = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int64.Parse
    (RunProgram program 0L).Count |> printfn "Part 1: %A"
    let registration = RunProgram program 1L
    let points = seq { for i in 0 .. 5 -> seq { for j in 1 .. 40 -> (i, j) } }
    points |> Seq.map(fun x -> x |> Seq.map(ConvertToChar registration) |> String.concat "" ) |> Seq.toArray |> printfn "%A"
    0 // return an integer exit code
