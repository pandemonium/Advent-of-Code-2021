module AdventOfCode.Day6

open System

type School = Fesh list

and Fesh = int

module Fesh =
  let spawnAndReset = [ 6; 8 ]
  let live x         = [ x - 1 ]

module School =
  let parse (input : string) : School =
    input.Split [|','|]
    |> List.ofArray
    |> List.map int

module Simulation =
  let tick (school : School) : School =
    let fesh =
      function age when age = 0 -> Fesh.spawnAndReset
             | age              -> Fesh.live age
    in List.collect fesh school

  let run days school : int =
    [ 1..days ]
    |> List.fold (fun s _ -> tick s) school
    |> List.length

module Test =
  let input = 
    "3,4,3,1,2"

module Live =
  let input =
    IO.File.ReadAllText "input-6.txt"

let compute = 
  Live.input
  |> School.parse
  |> Simulation.run 80