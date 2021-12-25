module AdventOfCode.Day5

open System


type Line =
  { P : Point; Q : Point }

and Point =
  { X : int; Y : int }


type Image = Map<Point, int>

module Image =
  let empty : Image = Map.empty

  let range r s =
    if r > s then [ r .. -1 .. s ] else [ r .. s ]

  let coverDiagonals p q =
    let xs = List.zip <| range p.X q.X
                      <| range p.Y q.Y
    in List.map (fun (x, y) -> { X = x; Y = y }) xs

  let ignoreDiagonals p q =
    []

  let coveredPoints diagonals (line : Line) : Point list =
    let vertical p q =
      [ for i in range p.Y q.Y do yield { X = p.X; Y = i } ]

    let horizontal p q =
      [ for i in range p.X q.X do yield { X = i; Y = p.Y } ]

    match line with
    | { P = p; Q = q } when p.X = q.X -> vertical p q
    | { P = p; Q = q } when p.Y = q.Y -> horizontal p q
    | { P = p; Q = q }                -> diagonals p q

  let overlaps diagonals =
    List.collect (coveredPoints diagonals)
    >> List.countBy id
    >> List.choose (snd >> function x when x >= 2 -> Some x | _ -> None)
    >> List.length


module Parse =
  let point (text : string) : Point =
    text.Split [|','|] 
    |> function [| x; y |] -> { X = int x; Y = int y } | _ -> failwith "Doh!"

  let line (text : string) : Line =
    text.Split ([|' '; '-'; '>'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> function [| p; q |] -> { P = point p; Q = point q } | _ -> failwith "Doh!"

  let lines =
    List.map line


module Test =
  let input =
    [ "0,9 -> 5,9"
      "8,0 -> 0,8"
      "9,4 -> 3,4"
      "2,2 -> 2,1"
      "7,0 -> 7,4"
      "6,4 -> 2,0"
      "0,9 -> 2,9"
      "3,4 -> 1,4"
      "0,0 -> 8,8"
      "5,5 -> 8,2" ]


module Live =
  let input =
    IO.File.ReadLines "input-5.txt"
    |> List.ofSeq

let compute diagonals =
  Live.input
  |> Parse.lines
  |> Image.overlaps diagonals

let compute1 =
  compute Image.ignoreDiagonals

let compute2 =
  compute Image.coverDiagonals