module AdventOfCode.Day1

open System


let compute =
  IO.File.ReadLines "input-1.txt" 
  |> List.ofSeq  
  |> List.map int
  |> List.pairwise
  |> List.countBy (fun (a, b) -> a < b)
  |> List.pick (function true, x -> Some x | _ -> None)