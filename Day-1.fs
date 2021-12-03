module AdventOfCode.Day1

open System


let count =
  List.pairwise
  >> List.countBy (fun (a, b) -> a < b)
  >> List.pick (function true, x -> Some x | _ -> None)

let input =
//  [ "199"; "200"; "208"; "210"; "200"; "207"; "240"; "269"; "260"; "263" ]
  IO.File.ReadLines "input-1.txt"
  |> List.ofSeq
  |> List.map int

let compute1 =
  input
  |> count

let compute2 =
  input
  |> List.windowed 3
  |> List.map List.sum
  |> count
