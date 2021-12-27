module AdventOfCode.Day7

open System

type Cluster = Position list

and Position = int

module Cluster =
  let parse (text : string) : Cluster =
    text.Split "," 
    |> Array.map int 
    |> List.ofArray

  let distanceSum (cluster : Cluster) (convergeance : Position) =
    List.sumBy (fun pos -> abs (convergeance - pos)) cluster

  let optimalCost (cluster : Cluster) =
    [ List.min cluster .. List.max cluster ]
    |> List.map (distanceSum cluster)
    |> List.min

module Test =
  let input = 
    "16,1,2,0,4,2,7,1,2,14"

module Live =
  let input =
    IO.File.ReadAllText "input-7.txt"

let compute =
  Live.input
  |> Cluster.parse
  |> Cluster.optimalCost