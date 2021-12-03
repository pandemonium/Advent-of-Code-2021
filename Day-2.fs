module AdventOfCode.Day2

open System


type Move = Forward of int
          | Up      of int
          | Down    of int

and Position =
  { Depth      : int
    Horizontal : int }


module Position =
  let intial =
    { Horizontal = 0; Depth = 0 }

  let product { Horizontal = h; Depth = d } =
    h * d

  let forward x pos =
    { pos with Horizontal = pos.Horizontal + x }

  let up x pos =
    { pos with Depth = pos.Depth - x }

  let down x pos =
    { pos with Depth = pos.Depth + x }


module Move =
  let decode (command : string) =
    match command.Split ' ' with
    | [| "forward"; by |] -> Some <| Forward (int by)
    | [| "up";      by |] -> Some <| Up      (int by)
    | [| "down";    by |] -> Some <| Down    (int by)
    | otherwise           -> None

  let apply =
    function Forward by -> Position.forward by
           | Up      by -> Position.up by
           | Down    by -> Position.down by


module Submarine =
  let test =
    [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]

  let travel (commands : string list) =
    commands
    |> List.choose Move.decode
    |> List.fold (fun position move -> Move.apply move position) Position.intial
    |> Position.product

let compute =
  IO.File.ReadLines "input-2.txt"
  |> List.ofSeq
  |> Submarine.travel