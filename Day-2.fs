module AdventOfCode.Day2

open System

let compute =
//  [ "199"; "200"; "208"; "210"; "200"; "207"; "240"; "269"; "260"; "263" ]
  IO.File.ReadLines "input-1.txt" 
  |> List.ofSeq
  |> List.map int
  |> List.windowed 3
  |> List.map List.sum
  |> List.pairwise
  |> List.countBy (fun (a, b) -> a < b)
  |> List.pick (function true, x -> Some x | _ -> None)