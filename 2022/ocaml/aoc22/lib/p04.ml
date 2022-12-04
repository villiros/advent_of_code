(*
 * Day 04
 *)

open Common

(* Single assignment *)
type assign = { left : int; right : int }
(* Line of input: pair of elf assignemns *)
type pair = assign * assign

(* A "null" assignment *)
let empty = { left = -1; right = -1 }

let intersect first second =
  let sr = max first.left second.left in
  let er = min first.right second.right in
  if sr <= er then { left = sr; right = er } else empty

let parse s =
  match List.map int_of_string (String.split_on_char '-' s) with
  | [ a; b ] -> { left = a; right = b }
  | _ -> assert false

let read_data ic : pair list =
  let rec reader acc =
    match input_line ic with
    | exception End_of_file -> acc
    | ln ->
      match List.map parse (String.split_on_char ',' ln) with
      | [a; b] -> reader ((a, b) :: acc)
      | _ -> assert false
  in
  List.rev (reader [])

let parta data =
  let check_one (a, b) =
    match intersect a b with
    | x when x = a || x = b -> 1
    | _ -> 0
  in
  List.fold_left ( + ) 0 (List.map check_one data)

let partb data =
  let check_one = function
    | (a, b) when intersect a b != empty -> 1
    | _ -> 0
  in
  List.fold_left ( + ) 0 (List.map check_one data)

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> parta (read_data ic)
  | PartB -> partb (read_data ic)
