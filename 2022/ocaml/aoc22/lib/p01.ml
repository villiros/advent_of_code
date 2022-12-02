(*
 * Day 01
 *)

open Common

(* Read from channel into a list of lists of integers (one least for each elf)*)
let read_data ic =
  let rec reader cur acc =
    match input_line ic with
    | exception End_of_file -> cur :: acc
    | "" -> reader [] (cur :: acc)
    | cals -> reader (int_of_string cals :: cur) acc
  in
  reader [] []

(* Part A *)
let parta data =
  let open List in
  let summed = map (fold_left ( + ) 0) data in
  hd (rev (sort ( - ) summed))

(* Part B *)
let partb data =
  let open List in
  let summed = map (fold_left ( + ) 0) data in
  match rev (sort ( - ) summed) with
  | a :: b :: c :: _ -> a + b + c
  | _ -> assert false

let solve part ic =
  match part with
  | PartA -> parta (read_data ic)
  | PartB -> partb (read_data ic)
