(*
 * Day 03
 *)

open Common

let ( << ) f g x = f (g x)
let apply2 f (a, b) = f a b

(* 2- and 3-tuple map *)
let map2 f (a, b) = (f a, f b)
let map3 f (a, b, c) = (f a, f b, f c)

let priority = function
  | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 1
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 27
  | _ -> assert false

(* Read into list of (left:int list, right: int list) with priorities resolved *)
let read_data ic =
  let open String in
  let rec reader acc =
    match input_line ic with
    | exception End_of_file -> acc
    | ln ->
        let to_list s = fold_right (fun c acc -> priority c :: acc) s [] in
        let sack_size = length ln / 2 in
        let left = sub ln 0 sack_size in
        let right = sub ln sack_size sack_size in
        reader ((to_list left, to_list right) :: acc)
  in
  List.rev (reader [])

let parta data =
  let module SS = Set.Make (Int) in
  (* Convert each tuple part into a set and then intersect them *)
  let find_disj a = SS.choose (apply2 SS.inter (map2 SS.of_list a)) in
  List.fold_left ( + ) 0 (List.map find_disj data)

let partb (data : (int list * int list) list) =
  let module SS = Set.Make (Int) in
  let solve_group a b c =
    (* For each tuple, concatenate two parts and make a set *)
    let a, b, c = map3 (SS.of_list << apply2 ( @ )) (a, b, c) in
    SS.choose (SS.inter a (SS.inter b c))
  in
  let rec solve = function
    | a :: b :: c :: rest -> solve_group a b c + solve rest
    | [] -> 0
    | _ -> assert false
  in
  solve data

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> parta (read_data ic)
  | PartB -> partb (read_data ic)
