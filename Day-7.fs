module AdventOfCode.Day7

open System

type Cluster = Position list

and Position = int

module Cluster =
  let parse (text : string) : Cluster =
    text.Split "," 
    |> Array.map int 
    |> List.ofArray

  let linear = id

  let nonLinear distance =
    let d = float distance
    in (d / 2.) * (1. + d)
       |> int

  let fuelCost calculate (cluster : Cluster) (convergeance : Position) =
    List.sumBy (fun pos -> calculate (abs (convergeance - pos))) cluster

  let optimalCost pricing (cluster : Cluster) =
    [ List.min cluster .. List.max cluster ]
    |> List.map (fuelCost pricing cluster)
    |> List.min

module Test =
  let input = 
    "16,1,2,0,4,2,7,1,2,14"

module Live =
  let input =
    IO.File.ReadAllText "input-7.txt"

let compute strategy =
  Live.input
  |> Cluster.parse
  |> Cluster.optimalCost strategy

let compute1 =
  compute Cluster.linear

let compute2 =
  compute Cluster.nonLinear