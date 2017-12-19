[<AutoOpen>]
module AOC.Common

open System
open System.Text.RegularExpressions

let asciiBytesToInts =
  Array.map (fun b -> (b - 48uy) |> int)

let rotateArrayBy offset (src:'a []) =
    let dest = Array.zeroCreate src.Length
    Array.Copy(src, offset, dest, 0, src.Length - offset)
    Array.Copy(src, 0, dest, src.Length - offset, offset)
    dest

let rotateArray ar =
    rotateArrayBy 1 ar

let numbersRowToList =
  let numberRegex = Regex(@"\d+", RegexOptions.Compiled)
  
  numberRegex.Matches 
  >> Seq.cast<Match>
  >> Seq.map (fun m -> m.Value |> Int32.Parse )
  >> Seq.toList
