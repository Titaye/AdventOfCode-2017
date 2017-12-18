module AOC.Day06

open Expecto
open System
open System.Collections.Generic

let puzzleInput = [| 4;10;4;1;8;4;9;14;5;1;14;15;0;15;3;5 |]

let distributeBanks (banks:int array) = 
  let (maxIdx,blocks) = 
    banks 
    |> Seq.mapi (fun i x -> i, x )
    |> Seq.sortByDescending (fun (i,x) -> x)
    |> Seq.head

  let incIdx i =
    (i + 1) % banks.Length

  let rec distribute blocks idx =
    if blocks = 0 then ()
    else 
      banks.[idx] <- banks.[idx] + 1
      distribute (blocks - 1) (idx |> incIdx)

  banks.[maxIdx] <- 0
  distribute blocks (maxIdx |> incIdx)
  banks |> Seq.toArray

//[| 0; 2; 7; 0 |] |> distributeBanks
let distributeUntilLoop (banks:int array) =
  let dict = System.Collections.Generic.Dictionary<String, int>()

  let rec distribute step =
    let state = banks |> distributeBanks |> sprintf "%A"
    match dict.ContainsKey(state) with
    | true -> step, dict.[state]
    | false -> 
        dict.Add(state, step)
        distribute (step + 1)
        
  distribute 1


[<Tests>]
let tests =
  testList "day06" [
    testList "puzzle1" [
      testCase "distribute banks" <| fun _ ->
        let result = [| 0; 2; 7; 0 |] |> distributeBanks
        Expect.equal result [| 2; 4; 1; 2 |] "distributeBanks error"

      testCase "puzzle inputs" <| fun _ ->
        let (steps, _stateIdx) = [| 0; 2; 7; 0 |] |> distributeUntilLoop
        
        Expect.equal steps 5 "Puzzle input not validated"


      testCase "puzzle answer" <| fun _ ->
        let input = puzzleInput |> Array.copy
        let (steps, _stateIdx) =
          input 
          |> distributeUntilLoop
        steps
        |> printfn "Day06 puzzle1 answer: %i "
    ]

    testList "puzzle2" [
      testCase "puzzle inputs" <| fun _ ->
        let (steps, stateIdx) = [| 0; 2; 7; 0 |] |> distributeUntilLoop
        
        Expect.equal (steps - stateIdx) 4 "Puzzle input not validated"


      testCase "puzzle answer" <| fun _ ->
        let input = puzzleInput |> Array.copy
        let (steps, stateIdx) =
          input 
          |> distributeUntilLoop
        
        (steps - stateIdx)
        |> printfn "Day06 puzzle2 answer: %i "

    ]
  ]