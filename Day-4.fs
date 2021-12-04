module AdventOfCode.Day4

open System


type BingoSystem =
  { Input  : int list
    Number : int
    Boards : Board list
    Winner : Board option }

and Board =
  { Numbers : int list
    Marked  : int Set }


module Board =
  let make numbers =
    { Numbers = numbers; Marked = Set.empty }

  let markNumber index board =
    { board with Marked = Set.add index board.Marked }

  let mark x board : Board =
    match List.tryFindIndex ((=) x) board.Numbers with
    | Some index -> markNumber index board
    | None       -> board

  let hasBingo board : bool =
    let row x =
      List.map (fun i -> 5 * x + i) [ 0..4 ]

    let column x =
      List.map (fun i -> i * 5 + x) [ 0..4 ]

    let strips =
      List.map row [ 0..4 ] @ List.map column [ 0..4 ]

    let bingo =
      List.forall (fun x -> Set.contains x board.Marked)
    in List.exists bingo strips

  let score number board : int =
    board.Numbers
    |> List.indexed
    |> List.filter (fun (i, _) -> not <| Set.contains i board.Marked)
    |> List.sumBy snd
    |> (*) number


module BingoSystem =
  let make numbers boards =
    { Input  = List.tail numbers
      Number = List.head numbers
      Boards = boards
      Winner = None }

  let parseInput (data : string list) : BingoSystem =
    let parseNumbers (text : string) : int list =
      text.Split ','
      |> Array.map int
      |> List.ofArray

    let split (t : string) =
      t.Split (' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

    let parseBoards =
      let parseBoard =
        List.take 5
        >> List.collect (split >> List.ofArray)
        >> List.map int
        >> Board.make
      in List.chunkBySize 6
         >> List.map (List.tail >> parseBoard)

    match data with
    | numbers::boards -> 
      make <| parseNumbers numbers
           <| parseBoards boards
    | otherwise -> failwithf "%A" otherwise

  let markBoards system =
    { system with Boards = List.map (Board.mark system.Number) system.Boards }

  let drawNumber system =
    { system with Number = List.head system.Input
                  Input  = List.tail system.Input }

  let findWinner system : Board option =
    List.tryFind Board.hasBingo system.Boards

  let rec play system =
    let system' =
      system
      |> markBoards

    match findWinner system' with
    | Some winner -> { system' with Winner = Some winner }
    | None        -> system' |> drawNumber |> play

  let score system =
    Option.map (Board.score system.Number) system.Winner

  let game =
    parseInput >> play >> score


module Test =
  let input =
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"; ""; "22 13 17 11  0"; " 8  2 23  4 24"; "21  9 14 16  7"; " 6 10  3 18  5"; " 1 12 20 15 19"; ""; " 3 15  0  2 22"; " 9 18 13 17  5"; "19  8  7 25 23"; "20 11 10 24  4"; "14 21 16 12  6"; ""; "14 21 17 24  4"; "10 16 15  9 19"; "18  8 23 26 20"; "22 11 13  6  5"; " 2  0 12  3  7" ]

module Live =
  let input = 
    IO.File.ReadLines "input-4.txt"
    |> List.ofSeq

let compute =
  Live.input
  |> BingoSystem.game