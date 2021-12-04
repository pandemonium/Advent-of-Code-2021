module AdventOfCode.Day3

open System


let input =
  IO.File.ReadLines "input-3.txt"
  |> List.ofSeq
//  [ "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010" ]

let frequencies bits =
  let ones, zeroes = List.partition ((=) '1') bits
  in List.length ones, List.length zeroes

let number (xs : char list) =
  Convert.ToInt32 (List.toArray xs |> String, 2)

let mostCommon =
  function a, b when a >= b -> '1' | _ -> '0'

let leastCommon =
  function a, b when a < b -> '1' | _ -> '0'

let compute1 =
  let diagnosticReport =
    input
    |> List.map List.ofSeq
    |> List.transpose

  let rate select =
    diagnosticReport
    |> List.map (frequencies >> select)
    |> number
  in rate mostCommon * rate leastCommon

let compute2 =
  let diagnosticReport =
    input
    |> List.map List.ofSeq

  let rec loop ix report select =
    let mask =
      report
      |> List.transpose
      |> List.map (frequencies >> select)
      |> List.item ix

    report
    |> List.filter (fun bitmap -> (List.item ix bitmap) = mask)
    |> function single :: [] -> number single
              | remaining    -> loop (ix + 1) remaining select
    
  let oxygenGeneratorRating =
    loop 0 diagnosticReport mostCommon
  let co2ScrubberRating =
    loop 0 diagnosticReport leastCommon
  in co2ScrubberRating * oxygenGeneratorRating
