(*
 * Day 12
 *)

open Common

type tpos = { r : int; c : int } (* row column *)
type tsensor = { pos : tpos; beacon : tpos }

let dist { r = r1; c = c1 } { r = r2; c = c2 } = abs (r2 - r1) + abs (c2 - c1)

type trange = { l : int; r : int } (* left right*)

let empty_range = { l = 0; r = -1 }
let range_size { l; r } = r - l

(* merges ranges, if possible *)
let range_merge ({ l = l1; r = r1 } as a) ({ l = l2; r = r2 } as b) =
  (* Cover cases where they are adjacent *)
  if r1 + 1 = l2 then [ { l = l1; r = r2 } ]
  else if r2 + 1 = l1 then [ { l = l2; r = r1 } ]
  else
    (* Overlaps and covers *)
    match (l2 < l1, l2 <= r1, r2 < l1, r2 <= r1) with
    | true, _, true, _ -> [ a; b ]
    | true, _, false, true -> [ { l = l2; r = r1 } ]
    | true, _, false, false -> [ b ]
    | false, true, _, true -> [ a ]
    | false, true, _, _ -> [ { l = l1; r = r2 } ]
    | false, false, _, _ -> [ a; b ]

let is_disj a b = match range_merge a b with [ _; _ ] -> true | _ -> false

(* Range in row that has been covered by the sensor. *)
let get_covered_range row sen =
  let sen_radius = dist sen.pos sen.beacon in
  let remain = sen_radius - abs (row - sen.pos.r) in
  logv (sen, sen_radius, remain);
  if remain < 0 then empty_range
  else { l = sen.pos.c - remain; r = sen.pos.c + remain }

let parse_regexp =
  Str.regexp
    {|Sensor at x=\([-0-9]+\), y=\([-0-9]+\): closest beacon is at x=\([-0-9]+\), y=\([-0-9]+\)|}

let read_data ic =
  (* In part A, this is the row being tested, added manually to the file. *)
  let target_row = int_of_string (input_line ic) in
  (* In part B, defines max coordinate, added manually to the file. *)
  let max_coord = int_of_string (input_line ic) in
  let rec reader () =
    match input_line ic with
    | exception End_of_file -> []
    | ln ->
        let found = Str.string_match parse_regexp ln 0 in
        assert found;
        let grab n = int_of_string (Str.matched_group n ln) in
        let el =
          {
            pos = { r = grab 2; c = grab 1 };
            beacon = { r = grab 4; c = grab 3 };
          }
        in
        el :: reader ()
  in
  (target_row, max_coord, reader ())

(* Get a (minimized) list of ranges that are covered by sensors in sens in row *)
let get_covered_ranges row sens =
  let proc_one ranges sen =
    let cur = get_covered_range row sen in
    if cur <> empty_range then
      let last, ranges =
        List.fold_left
          (fun (r, acc) e ->
            match range_merge r e with
            | [ n ] -> (n, acc)
            | [ _; _ ] -> (r, e :: acc)
            | _ -> assert false)
          (cur, []) ranges
      in
      last :: ranges
    else ranges
  in
  let ranges = List.fold_left proc_one [] sens in
  let assert_disj e lst =
    List.iter (fun i -> assert (e = i || (is_disj e i && is_disj i e))) lst
  in
  (* Check the result is all disjoint ranges *)
  List.iter (fun e -> assert_disj e ranges) ranges;
  logv ranges;
  ranges

let parta (target, _, sens) =
  let ranges = get_covered_ranges target sens in
  List.fold_left ( + ) 0 (List.map range_size ranges)

let partb (_, max_coord, sens) =
  (* Brute-force is fast enough here *)
  let result = ref 0 in
  let i = ref ~-1 in
  while !i <= max_coord && !result = 0 do
    i := !i + 1;
    match get_covered_ranges !i sens with
    | [ _ ] -> ()
    | [ { r = r1; _ }; { l = l2; _ } ] when r1 + 2 = l2 ->
        result := ((r1 + 1) * 4000000) + !i
    | [ { l = l1; _ }; { r = r2; _ } ] when r2 + 2 = l1 ->
        result := ((r2 + 1) * 4000000) + !i
    | _n ->
        logv _n;
        assert false
  done;
  !result

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
