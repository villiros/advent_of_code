(*
 * Day 20
 *)

open Common

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

(* Doubly-linked list *)
module DLinkList = struct
  type tnode = { mutable prev : tnode ref; mutable next : tnode ref; v : int }
  type t = tnode ref

  let insert_after node v =
    let prev = node in
    let next = !node.next in
    let nnode = ref { prev; next; v } in
    !prev.next <- nnode;
    !next.prev <- nnode;
    nnode

  let insert_before node v = insert_after !node.prev v

  let remove node =
    let v = !node.v in
    let prev = !node.prev in
    let next = !node.next in

    !prev.next <- next;
    !next.prev <- prev;
    (next, v)

  let put_back removed before_node =
    let prev = !before_node.prev in

    !prev.next <- removed;
    !removed.prev <- prev;
    !removed.next <- before_node;
    !before_node.prev <- removed

  let skip node n =
    let p = ref node in
    if n >= 0 then
      for _ = 1 to n do
        p := !(!p).next
      done
    else
      for _ = n to -1 do
        p := !(!p).prev
      done;
    !p

  let find_val node v =
    let rec finder n = if !n.v == v then n else finder !n.next in
    finder node

  let to_finger_list node =
    let rec builder n =
      n :: (if !n.next != node then builder !n.next else [])
    in
    builder node

  let make vlist =
    match vlist with
    | first :: rest ->
        let rec first_node =
          ref { prev = first_node; next = first_node; v = first }
        in
        ignore (List.fold_left insert_after first_node rest);
        first_node
    | _ -> assert false

  let to_list node =
    let rec builder n =
      !n.v :: (if !n.next != node then builder !n.next else [])
    in
    builder node
end

let rec read_data ic =
  match input_line ic with
  | exception End_of_file -> []
  | n -> int_of_string n :: read_data ic

open DLinkList

let do_mix fl len =
  List.iter
    (fun n ->
      let next, v = remove n in
      let next = skip next (v mod (len - 1)) in
      put_back n next)
    fl

let parta data =
  let cl = make data in
  let len = List.length data in
  let fl = to_finger_list cl in
  do_mix fl len;

  let start = find_val cl 0 in
  let first = skip start 1000 in
  let second = skip start 2000 in
  let third = skip start 3000 in
  !first.v + !second.v + !third.v

let partb data =
  let data = List.map (( * ) 811589153) data in
  let cl = make data in
  let len = List.length data in
  let fl = to_finger_list cl in

  List.iter (fun _ -> do_mix fl len) (seq 1 10);
  let start = find_val cl 0 in
  let first = skip start 1000 in
  let second = skip start 2000 in
  let third = skip start 3000 in
  !first.v + !second.v + !third.v

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
