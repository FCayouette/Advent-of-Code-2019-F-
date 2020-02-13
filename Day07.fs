module List = 
    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
        and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

open System.IO;

let GetFirst (x : int[]) y =
    match ((x.[y]/100)%10) with
        | 0 -> x.[x.[y+1]]
        | _ -> x.[y+1]

let GetSecond (x : int[]) y =
    match ((x.[y]/1000)%10) with
        | 0 -> x.[x.[y+2]]
        | _ -> x.[y+2]

// Returns the following tuple
// (stopCode, value, program, PC)
let rec IntcodeAdd (x : int[]) y input = 
    x.[x.[y+3]] <- (GetFirst x y) + (GetSecond x y)
    IntcodeProcess x (y+4) input

and IntcodeMul (x : int[]) y input = 
    x.[x.[y+3]] <- (GetFirst x y) * (GetSecond x y)
    IntcodeProcess x (y+4) input

and IntcodeGet (x : int[]) y (input:List<int>) =
    match input with
    | [] -> (0, 0, x, y)
    | h::t -> x.[x.[y+1]] <- h  
              IntcodeProcess x (y+2) t

and IntcodePrint (x : int[]) y input =
    (1, (GetFirst x y), x, (y+2))

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
        | _ -> (2, List.head input, x, (y+1))


let first (a, _, _, _) = a
let second (_, b, _, _) = b
let third (_, _, c, _) = c
let fourth (_, _, _, d) = d

let rec amp program p initialVal =
    match p with
    | [] -> initialVal
    | x::xs -> amp program xs (second (IntcodeProcess (program |> Seq.toArray) 0 [x; initialVal]))

let rec RunPrograms x y value =
   match x with
   | [] -> match first(List.head y) with
        | 2 -> value
        | _ -> RunPrograms y List.empty value
   | x::xs -> let nextState = IntcodeProcess (third x) (fourth x) [value]
              RunPrograms xs (List.append y [nextState]) (second nextState)

let part2 program p =
    let initializedPrograms = Seq.map (fun x -> IntcodeProcess (program |> Seq.toArray) 0 [x]) p |> Seq.toList
    RunPrograms initializedPrograms List.empty 0

[<EntryPoint>]
let main argv =

    let program = File.ReadAllText(argv.[0]).Split(',') |> Seq.map System.Int32.Parse 
    let results = Seq.map (fun x -> amp program x 0 ) (List.permutations [0; 1; 2; 3; 4]) |> Seq.max
    printfn "Part 1: %A" results 
    
    let part2Results = Seq.map (fun x -> part2 program x) (List.permutations [5; 6; 7; 8; 9]) |> Seq.max
    printfn "Part 2: %d" part2Results
    0 // return an integer exit code
