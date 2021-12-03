module AdventOfCode.Day3

open System

let input =
  IO.File.ReadLines "input-3.txt"
  |> List.ofSeq

//  [ "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010" ]

//let mostCommonBit 

let compute =
  let diagnosticReport =
    input
    |> List.map List.ofSeq
    |> List.transpose

  let frequencies bits =
    let ones, zeroes = List.partition ((=) '1') bits
    in List.length ones, List.length zeroes

  let gamma =
    function a, b when a > b -> '1' | _ -> '0'

  let epsilon =
    function a, b when a < b -> '1' | _ -> '0'

  let rate select =
    diagnosticReport
    |> List.map (frequencies >> select)
    |> List.toArray
    |> fun xs -> Convert.ToInt32 (String xs, 2)
  in rate gamma * rate epsilon