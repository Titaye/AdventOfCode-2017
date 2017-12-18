module AOC.Day03

open Expecto
open System

let computeDistance input =
  let rec compute input i previousSquare = 
    let size = (i * 2 + 1)
    let distanceFromCenterToSide = i
    let distanceFromCornerToMiddleOfSide = i
    let square = size * size
    if square >= input then
      let squareSize = square - previousSquare
      // printfn "total side length %A" squareSize

      let positionToEndInThisSquare = square - input
      // printfn "positionToEndInThisSquare %A %A" positionToEndInThisSquare (positionToEndInThisSquare % size)

      let quadrant = int <| (float positionToEndInThisSquare) / (float squareSize) * 4.
      // printfn "quadrant %A" quadrant
      
      let middleOfSideValue = square - (quadrant * size + distanceFromCornerToMiddleOfSide - quadrant)
      // printfn "middleOfSideValue %A"  middleOfSideValue

      let taxicab = Math.Abs(input - middleOfSideValue) + distanceFromCenterToSide
      taxicab
    else 
      compute input (i + 1) square

  compute input 0 1

type Direction =
| North
| South
| East
| West

type Square () =
  let mutable w = None
  let mutable nw = None
  let mutable n = None
  let mutable ne = None
  let mutable e = None
  let mutable se = None
  let mutable s = None
  let mutable sw = None
  
  member val Value = 0 with get, set
  member val Id = 0 with get, set
  member this.N 
    with get () : Square option = n
    and set (v) =
      n <- v
      match v with 
      | Some square when square.S |> Option.isNone -> square.S <- Some this
      | _ -> ()

  member this.NW 
    with get () : Square option = nw
    and set (v) =
      nw <- v
      match v with 
      | Some square when square.SE |> Option.isNone -> square.SE <- Some this
      | _ -> ()
  
  member this.NE
    with get () : Square option = ne
    and set (v) =
      ne <- v
      match v with 
      | Some square when square.SW |> Option.isNone -> square.SW <- Some this
      | _ -> ()
  
  member this.E
    with get () : Square option = e
    and set (v) =
      e <- v
      match v with 
      | Some square when square.W |> Option.isNone -> square.W <- Some this
      | _ -> ()

  member this.SE
    with get () : Square option = se
    and set (v) =
      se <- v
      match v with 
      | Some square when square.NW |> Option.isNone -> square.NW <- Some this
      | _ -> ()

  member this.S
    with get () : Square option = s
    and set (v) =
      s <- v
      match v with 
      | Some square when square.N |> Option.isNone -> square.N <- Some this
      | _ -> ()

  member this.SW
    with get () : Square option = sw
    and set (v) =
      sw <- v
      match v with 
      | Some square when square.NE |> Option.isNone -> square.NE <- Some this
      | _ -> ()

  member this.W
    with get () : Square option = w
    and set (v) =
      w <- v
      match v with 
      | Some square when square.E |> Option.isNone -> square.E <- Some this
      | _ -> ()


let walkDirection direction (square:Square) =
  match direction with
  | North -> square.N
  | South -> square.S
  | West -> square.W
  | East -> square.E

let squareValue = 
  List.choose (fun (s:Square option) -> 
    s |> function 
    | Some s -> Some s.Value
    | None -> None)
  >> List.sum

// let rec walkDirections directions (square) = 
//   match directions with
//   | direction :: tail -> 
//       square
//       |> walkDirection direction
//       |> function
//       | Some s -> walkDirections tail s
//       | None -> None
//   | [] -> Some square

let fillDirection squareId (square:Square) = function
  | East -> 
      let w = Some square 
      let nw = square |> walkDirection North
      let n = nw |> Option.bind (walkDirection East)
      let ne = n |> Option.bind (walkDirection East)
      let e = None
      let se = None
      let s = None
      let sw = None

      let newSquare = 
        Square(
          Id = squareId,
          Value = (squareValue [ w; nw; n; ne; e; se; s; sw ]),
          N = n, S = s, E = e, W = w,
          NE = ne, NW = nw, SE = se, SW = sw)
      
      let nextDirection = 
        if n |> Option.isSome then East
        else North
      newSquare, nextDirection
      
  | North -> 
      let s = Some square 
      let sw = square |> walkDirection West
      let w = sw |> Option.bind (walkDirection North)
      let nw = w |> Option.bind (walkDirection North)
      let n = None
      let ne = None
      let e = None
      let se = None

      let newSquare = 
        Square(
          Id = squareId,
          Value = (squareValue [ w; nw; n; ne; e; se; s; sw ]),
          N = n, S = s, E = e, W = w,
          NE = ne, NW = nw, SE = se, SW = sw)
      
      let nextDirection = 
        if w |> Option.isSome then North
        else West
      newSquare, nextDirection

  | South -> 
      let n = Some square 
      let ne = square |> walkDirection East
      let e = ne |> Option.bind (walkDirection South)
      let se = e |> Option.bind (walkDirection South)
      let s = None
      let sw = None
      let w = None
      let nw = None

      let newSquare = 
        Square(
          Id = squareId,
          Value = (squareValue [ w; nw; n; ne; e; se; s; sw ]),
          N = n, S = s, E = e, W = w,
          NE = ne, NW = nw, SE = se, SW = sw)
      
      let nextDirection = 
        if e |> Option.isSome then South
        else East
      newSquare, nextDirection

  | West -> 
      let e = Some square 
      let se = square |> walkDirection South
      let s = se |> Option.bind (walkDirection West)
      let sw = s |> Option.bind (walkDirection West)
      let w = None
      let nw = None
      let n = None
      let ne = None

      let newSquare = 
        Square(
          Id = squareId,
          Value = (squareValue [ w; nw; n; ne; e; se; s; sw ]),
          N = n, S = s, E = e, W = w,
          NE = ne, NW = nw, SE = se, SW = sw)
      
      let nextDirection = 
        if s |> Option.isSome then West
        else South
      newSquare, nextDirection

let rec fillUntil cond =
  let rec fillUntil square direction i =
    match cond square with
    | true -> square
    | false ->
        let (newSquare, nextDirection) = fillDirection i square direction
        // printfn "step %A direction %A" i nextDirection
        fillUntil newSquare nextDirection (i + 1)
  
  let root = Square(Id = 1, Value = 1 )
  fillUntil root East 2


[<Tests>]
let tests =
  testList "day03" [
    testList "puzzle1" [
      testCase "puzzle inputs" <| fun _ ->
        let tests = 
          [ 1, 0
            12, 3
            23, 2
            1024, 31 ]


        tests 
        |> List.iter ( fun (input,expected) ->
            let result = 
              input 
              |> computeDistance
            
            Expect.equal result expected (sprintf "Puzzle input %A test is failing" input) )

      testCase "puzzle answer" <| fun _ ->
        let input = 361527

        input 
        |> computeDistance
        |> printfn "Day03 puzzle1 answer: %i "
    ]

    testList "puzzle2" [
      testCase "puzzle inputs" <| fun _ ->
        let result = fillUntil (fun x -> x.Id = 17 )
        
        Expect.equal result.Value 147 "Puzzle input failed"

      testCase "puzzle answer" <| fun _ ->
        let input = 361527
        let result = fillUntil (fun x -> x.Value > input )

        printfn "Day03 puzzle2 answer: %i " result.Value

    ]
  ]