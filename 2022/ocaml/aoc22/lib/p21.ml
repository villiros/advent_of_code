(*
   Day 21
*)

open Common

type toper =
  | Const of int
  | Add of string * string
  | Sub of string * string
  | Mul of string * string
  | Div of string * string

type tmonkey = { name : string; mutable op : toper }

let rec read_data ic =
  match String.split_on_char ' ' (input_line ic) with
  | exception End_of_file -> []
  | [ names; consts ] ->
      let name = String.sub names 0 (String.length names - 1) in
      { name; op = Const (int_of_string consts) } :: read_data ic
  | [ names; lefts; op; rights ] ->
      let name = String.sub names 0 (String.length names - 1) in
      let op =
        match op with
        | "+" -> Add (lefts, rights)
        | "-" -> Sub (lefts, rights)
        | "*" -> Mul (lefts, rights)
        | "/" -> Div (lefts, rights)
        | _ -> assert false
      in
      { name; op } :: read_data ic
  | _ -> assert false

let rec resolve vars name =
  let value =
    match (Hashtbl.find vars name).op with
    | Add (l, r) -> resolve vars l + resolve vars r
    | Sub (l, r) -> resolve vars l - resolve vars r
    | Mul (l, r) -> resolve vars l * resolve vars r
    | Div (l, r) -> resolve vars l / resolve vars r
    | Const v -> v
  in
  value

let parta data =
  let vars =
    Hashtbl.of_seq (Seq.map (fun m -> (m.name, m)) (List.to_seq data))
  in
  resolve vars "root"

let rec bin_search vars svar target n range =
  (* This is not really a binary search: it assumes that n and n+range are on the same
     side of the target value. If not, it reduces the range. *)
  (Hashtbl.find vars "humn").op <- Const n;
  logv (n, range, resolve vars svar, target);
  let vn = resolve vars svar in
  let n2 = n + range in
  (Hashtbl.find vars "humn").op <- Const n2;
  let rn = resolve vars svar in
  if vn = target then n
  else if rn = target then n2
  else if (vn < target && rn < target) || (vn > target && rn > target) then
    bin_search vars svar target n2 range
  else
    let range = max 1 (range / 2) in
    bin_search vars svar target n range

let partb data =
  (* Simply do a binary search. *)
  let vars =
    Hashtbl.of_seq (Seq.map (fun m -> (m.name, m)) (List.to_seq data))
  in
  let left, right =
    match (Hashtbl.find vars "root").op with
    | Add (l, r) -> (l, r)
    | _ -> assert false
  in
  (* Optimistically assume that humn var is in the left branch *)
  bin_search vars left (resolve vars right) 0 1000000000

(*
   Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
