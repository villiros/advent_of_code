(*
   Day 19
*)

open Common

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

type tresource = Ore | Clay | Obsidian | Geode

let assoc_opt default key alist =
  match List.assoc_opt key alist with
  | None -> default
  | Some x -> x

let read_data ic =
  (* Cheat: it's always the same resources for all inputs *)
  let regexp =
    Str.regexp
      "Blueprint \\(.+\\): \
       Each ore robot costs \\([0-9]+\\) ore[.] \
       Each clay robot costs \\([0-9]+\\) ore[.] \
       Each obsidian robot costs \\([0-9]+\\) ore and \\([0-9]+\\) clay[.] \
       Each geode robot costs \\([0-9]+\\) ore and \\([0-9]+\\) obsidian" in
  let rec reader i =
    match input_line ic with
    | exception End_of_file -> []
    | ln ->
      let found = Str.string_match regexp ln 0 in
      logv ln;
      assert found;
      let toint ind = int_of_string (Str.matched_group ind ln) in
      assert (toint 1 = i);
      let el = [(Ore, [(Ore, toint 2)]);
                (Clay, [(Ore, toint 3)]);
                (Obsidian, [(Ore, toint 4); (Clay, toint 5)]);
                (Geode, [(Ore, toint 6); (Obsidian, toint 7)])] in
      el :: reader (i+1)
  in
  reader 1

let run_solver max_time blueprint =
  let assoc_opt = assoc_opt 0 in
  (* Row for the robot costs in the model for typ robot. *)
  let get_row typ =
    match List.assoc typ blueprint with
    | alist ->
      Format.sprintf "|%i,%i,%i,%i"
        (assoc_opt Ore alist)
        (assoc_opt Clay alist)
        (assoc_opt Obsidian alist)
        (assoc_opt Geode alist)
  in

  (* This assumes MiniZinc (worked with 2.6.4) is installed globally on osx *)
  (* Pretty crude: just run the solver with params on the cmd line, tell it
     to output to a file in tmp and then get the result from there. *)
  let minizinc = "/Applications/MiniZincIDE.app//Contents/Resources/minizinc" in
  let res_filename = "/tmp/p19a.out" in
  (* Assume run from ocaml/aoc22/ *)
  let cmd = Format.sprintf
    "%s --solver Chuffed ../../miniZinc/p19.mzn -o %s -D \"max_time=%i\" -D \"robot_costs = [%s%s%s%s|]\""
    minizinc
    res_filename
    max_time
    (get_row Ore) (get_row Clay) (get_row Obsidian) (get_row Geode)
  in
  logv cmd;
  (match Sys.command cmd with
  | 0 -> ()
  | _ -> assert false);
  let resstr = In_channel.with_open_bin res_filename In_channel.input_all in
  match String.split_on_char '\n' resstr with
  | [result; "----------"; "=========="; ""] ->
    int_of_string result
  | other ->
    logv other;
    assert false

let parta data =
  let res, _ = List.fold_left
    (fun (acc, i) x -> (acc + i * (run_solver 24 x), i + 1))
    (0, 1)
    data
  in
  res

let partb data =
  let data = List.map (List.nth data) (seq 1 3) in
  List.fold_left (fun acc x -> acc * (run_solver 32 x)) 1 data

(*
   Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
