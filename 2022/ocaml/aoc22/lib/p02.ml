(*
 * Day 02
 *)

open Common

(* Function composition *)
let ( << ) f g x = f (g x)

(* CHOOSE x \in (y list): f(x) *)
let rec choose y f =
  match y with
  | [] -> assert false
  | h :: _ when f h -> h
  | _ :: t -> choose t f

type move = Rock | Paper | Scissors

let moves = [ Rock; Paper; Scissors ]

type match_result = MyWin | MyLoss | Draw

(* Winning moves *)
let winning_move = function
  | Rock -> Scissors
  | Scissors -> Paper
  | Paper -> Rock

let match_result (opp, my) =
  match my with
  | _ when winning_move opp = my -> MyLoss
  | _ when opp = my -> Draw
  | _ ->
      assert (winning_move my = opp);
      MyWin

(* Move scores *)
let move_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

(* Read data into (opp_move_char, my_move_char) *)
let read_data ic =
  let rec reader acc =
    match input_line ic with
    | exception End_of_file -> acc
    | ln -> reader ((ln.[0], ln.[2]) :: acc)
  in
  reader []

let parta data =
  let conv = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | _ -> assert false
  in
  let conv_pair (a, b) = (conv a, conv b) in
  let result_score = function MyWin -> 6 | Draw -> 3 | MyLoss -> 0 in
  let match_score ((_, my) as m) =
    move_score my + result_score (match_result m)
  in
  List.fold_left ( + ) 0 (List.map (match_score << conv_pair) data)

(*
 * Part B
 *)

let partb data =
  let conv_move = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> assert false
  in
  let conv_result = function
    | 'X' -> MyLoss
    | 'Y' -> Draw
    | 'Z' -> MyWin
    | _ -> assert false
  in
  let conv_pair (a, b) = (conv_move a, conv_result b) in
  let result_to_move result opp =
    choose moves (fun my -> match_result (opp, my) = result)
  in
  let decide_move (a, b) = (a, result_to_move b a) in
  let result_score = function MyWin -> 6 | Draw -> 3 | MyLoss -> 0 in
  let match_score ((_, my) as m) =
    move_score my + result_score (match_result m)
  in
  List.fold_left ( + ) 0
    (List.map (match_score << decide_move << conv_pair) data)

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
