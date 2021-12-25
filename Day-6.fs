module AdventOfCode.Day6

open System
open System.Numerics
open System.Collections.Specialized


type School = int64 array

module School =
  let empty () = Array.create 9 0L

  let fromInitial =
    List.countBy id
    >> List.fold (fun (a : School) (k, v) -> a.[k] <- v; a) (empty ())

  let parse (input : string) : School =
    input.Split [|','|]
    |> List.ofArray
    |> List.map int
    |> fromInitial

module Simulation =
  let tick (ages : School) : School =
    let toSpawn = ages.[0]
    let ages' = School.empty ()
    Array.ConstrainedCopy (ages, 1, ages', 0, ages.Length - 1)

    ages'.[6] <- ages'.[6] + toSpawn
    ages'.[8] <- toSpawn
    ages'

  let run days school : int64 =
    [ 1..days ]
    |> List.fold (fun s _ -> tick s) school
    |> Array.sum

module Test =
  let input = 
    "3,4,3,1,2"

module Live =
  let input =
    IO.File.ReadAllText "input-6.txt"

let compute simulations =
  Live.input
  |> School.parse
  |> Simulation.run simulations

let compute1 =
  compute 80

let compute2 =
  compute 256