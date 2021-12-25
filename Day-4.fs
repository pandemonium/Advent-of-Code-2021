module AdventOfCode.Day4

open System


type BingoSystem =
  { Input  : int list
    Number : int
    Boards : Board list
    Streak : int list
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
      Streak = []
      Winner = None }

  let decode (input : string list) : BingoSystem =
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

    match input with
    | numbers::boards -> 
      make <| parseNumbers numbers
           <| parseBoards boards
    | otherwise -> failwithf "%A" otherwise

  let markBoards system =
    { system with Boards = List.map (Board.mark system.Number) system.Boards }

  let drawNumber system =
    { system with Number = List.head system.Input
                  Input  = List.tail system.Input }

  let findFirstWinner system : Board option =
    List.tryFind Board.hasBingo system.Boards

  let rec playUntilOneWinner before =
    let after = markBoards before

    match findFirstWinner after with
    | Some winner -> { after with Winner = Some winner }
    | None        -> after |> drawNumber |> playUntilOneWinner

  let bingoIndices =
    List.indexed
    >> List.filter (snd >> Board.hasBingo)
    >> List.map fst

  let rec playUntilAllWinners before : BingoSystem =
    let after = markBoards before
    let diff = 
      List.except <| bingoIndices before.Boards
                  <| bingoIndices after.Boards
    let streak = after.Streak @ diff

    if List.length streak = List.length after.Boards
      then { after with Streak = streak
                        Winner = List.last streak
                                 |> fun ix -> after.Boards.[ix]
                                 |> Some }
      else { after with Streak = streak }
           |> drawNumber
           |> playUntilAllWinners

  let score system =
    Option.map (Board.score system.Number) system.Winner

  let gameUntilFirst =
    decode >> playUntilOneWinner >> score

  let gameUntilLast =
    decode >> playUntilAllWinners >> score


module Test =
  let input =
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"; ""; "22 13 17 11  0"; " 8  2 23  4 24"; "21  9 14 16  7"; " 6 10  3 18  5"; " 1 12 20 15 19"; ""; " 3 15  0  2 22"; " 9 18 13 17  5"; "19  8  7 25 23"; "20 11 10 24  4"; "14 21 16 12  6"; ""; "14 21 17 24  4"; "10 16 15  9 19"; "18  8 23 26 20"; "22 11 13  6  5"; " 2  0 12  3  7" ]

module Live =
  let input = 
    IO.File.ReadLines "input-4.txt"
    |> List.ofSeq

let compute1 =
  Live.input
  |> BingoSystem.gameUntilFirst

let compute2 =
  Live.input
  |> BingoSystem.gameUntilLast