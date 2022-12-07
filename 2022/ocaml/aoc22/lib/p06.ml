(*
 * Day 6
 *)

open Common

(* Convert a string into a list of chars *)
let explode (s : string) =
  let sl = String.length s in
  let rec doit = function i when i = sl -> [] | i -> s.[i] :: doit (i + 1) in
  doit 0

(* Given a (index, list) return two lists split on index *)
let rec (split_at : int * 'a list -> 'a list * 'a list) = function
  | 0, rest -> ([], rest)
  | n, a :: rest ->
      let f, s = split_at (n - 1, rest) in
      (a :: f, s)
  | _ -> assert false

(*
 * Sliding window duplicates tracker
 * Add new characters (from the right) with add_char
 * Remove leftmost that's falling off with rem_char
 * has_dups will return false if there are no duplicates
 * Can also provide an initial window with initial list
 * Only works with a-z
 *)
class duper (initial : char list) =
  let num_chars = 26 in
  let char_to_index c = Char.code c - Char.code 'a' in
  object (self)
    (* For each character, keep count of them in the window.
       If any character is present more then once, then num_dups
       is also incremented
    *)
    val seen = Array.make num_chars 0
    val mutable num_dups = 0

    method add_char c =
      let c = char_to_index c in
      match seen.(c) with
      | 0 -> seen.(c) <- 1
      | _ ->
          seen.(c) <- seen.(c) + 1;
          num_dups <- num_dups + 1

    method rem_char c =
      let c = char_to_index c in
      assert (seen.(c) > 0);
      match seen.(c) with
      | 1 -> seen.(c) <- 0
      | n when n > 1 ->
          assert (num_dups > 0);
          seen.(c) <- seen.(c) - 1;
          num_dups <- num_dups - 1
      | _ -> assert false

    method has_dups = num_dups > 0
    initializer List.iter self#add_char initial
  end

let read_data ic =
  match input_line ic with
  | exception End_of_file -> assert false
  | s -> explode s

let solve part data =
  let wsz = match part with PartA -> 4 | PartB -> 14 in
  let first, _ = split_at (wsz, data) in
  let duper = new duper first in
  let extract = function
    | r :: _ :: _ :: _ :: n :: _ when part = PartA -> (r, n)
    (* :) *)
    | r :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: n
      :: _
      when part = PartB ->
        (r, n)
    | _ -> assert false
  in
  let rec solve i lst =
    let r, n = extract lst in
    duper#rem_char r;
    duper#add_char n;
    if duper#has_dups then solve (i + 1) (List.tl lst) else i + wsz
  in
  solve 1 data

(*
 * Runner
 *)

let solve part ic = Int (solve part (read_data ic))
