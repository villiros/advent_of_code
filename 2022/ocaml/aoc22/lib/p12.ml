(*
 * Day 12
 *)

open Common

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

(*
Simple generic and very naive implementation of Dijkstra search.

This is actually quite slow, as it keeps distances in a hash table and
(rather than using a matrix, which would've been appropriate for this problem).
It also does a flood to fill up the queue and distances on start.

Parametrized on a Node_id: a comparable identifier of a graph node.

find_shortest then expects a function that shows connections from each node.

This assumes that node distances are always 1

*)
module Dijkstra (Node_id : Set.OrderedType) = struct
  type t = Node_id.t

  (* Type for making a priority queue from a Set *)
  module QType = struct
    type t = int * Node_id.t

    let compare (dist1, node_id1) (dist2, node_id2) =
      match compare dist1 dist2 with
      | 0 -> Node_id.compare node_id1 node_id2
      | n -> n
  end

  (* Priority queue *)
  module Queue = Set.Make (QType)

  let inf = 1000000000

  (* Find a path from start to target. May return inf if none exists. *)
  let find_shortest (start : t) (target : t) (neighbours : t -> t list) : int =
    let queue = ref Queue.empty in
    let dist = Hashtbl.create 100 in
    let rec find_all_nodes n =
      match Queue.find (inf, n) !queue with
      | exception Not_found ->
          queue := Queue.add (inf, n) !queue;
          Hashtbl.replace dist n inf;
          List.iter find_all_nodes (neighbours n)
      | _ -> ()
    in
    List.iter find_all_nodes (neighbours start);
    queue := Queue.add (0, start) !queue;
    Hashtbl.replace dist start 0;

    (* In part B not all nodes are connected to target. XXX: can skip the whole thing in this case. *)
    (* assert (match Queue.find (inf, target) !queue with | exception Not_found -> false | _ -> true); *)
    while not (Queue.is_empty !queue) do
      let udist, u = Queue.min_elt !queue in
      queue := Queue.remove (udist, u) !queue;
      if u != target then
        List.iter
          (fun v ->
            let alt = Hashtbl.find dist u + 1 in
            let old_dist = Hashtbl.find dist v in
            if alt < old_dist then (
              queue := Queue.remove (old_dist, v) !queue;
              queue := Queue.add (alt, v) !queue;
              Hashtbl.replace dist v alt))
          (neighbours u)
      else queue := Queue.empty
    done;
    match Hashtbl.find dist target with n -> n | exception Not_found -> inf
end

module Grid = struct
  type t = int Array.t Array.t
  type tpos = { r : int; c : int } (* row column *)

  module PosComp = struct
    type t = tpos

    let compare { r = r1; c = c1 } { r = r2; c = c2 } =
      match compare r1 r2 with 0 -> compare c1 c2 | n -> n
  end

  let vstart = 0
  let vend = 27
  let make () : t = Array.init 0 (fun _ -> Array.make 0 0)
  let add_row g row = Array.append g (Array.make 1 (Array.of_list row))
  let max_row (g : t) : int = Array.length g - 1
  let max_col (g : t) : int = Array.length g.(0) - 1
  let at (g : t) (pos : tpos) = g.(pos.r).(pos.c)
  let set g pos v = g.(pos.r).(pos.c) <- v

  let is_valid g pos =
    pos.r >= 0 && pos.r <= max_row g && pos.c >= 0 && pos.c <= max_col g

  let all_pos (g : t) : tpos list =
    List.fold_left
      (fun acc r ->
        List.fold_left (fun acc c -> { r; c } :: acc) acc (seq 0 (max_col g)))
      []
      (seq 0 (max_row g))

  let neighbours g pos =
    let add { r = r1; c = c1 } { r = r2; c = c2 } =
      { r = r1 + r2; c = c1 + c2 }
    in
    List.fold_left
      (fun acc step ->
        let p = add pos step in
        if is_valid g p then p :: acc else acc)
      []
      [
        { r = 1; c = 0 }; { r = -1; c = 0 }; { r = 0; c = 1 }; { r = 0; c = -1 };
      ]
end

let read_data ic =
  let to_height = function
    | 'S' -> Grid.vstart
    | 'E' -> Grid.vend
    | 'a' .. 'z' as c -> int_of_char c - 97 + 1
    | _ -> assert false
  in
  let data = ref (Grid.make ()) in
  let rec reader () =
    match input_line ic with
    | exception End_of_file -> ()
    | ln ->
        data :=
          Grid.add_row !data
            (String.fold_right (fun c acc -> to_height c :: acc) ln []);
        reader ()
  in
  reader ();
  !data

let parta data =
  let start =
    List.find (fun pos -> Grid.at data pos == Grid.vstart) (Grid.all_pos data)
  in
  let target =
    List.find (fun pos -> Grid.at data pos == Grid.vend) (Grid.all_pos data)
  in

  let valid_steps pos =
    List.filter
      (fun p -> Grid.at data pos + 1 >= Grid.at data p)
      (Grid.neighbours data pos)
  in

  log (fun _ -> Dum.to_stdout (start.r, start.c, target.r, target.c));

  let module Solver = Dijkstra (Grid.PosComp) in
  Solver.find_shortest start target valid_steps

let partb data =
  let start =
    List.find (fun pos -> Grid.at data pos == Grid.vstart) (Grid.all_pos data)
  in
  let target =
    List.find (fun pos -> Grid.at data pos == Grid.vend) (Grid.all_pos data)
  in

  (* Reset start to 'a' *)
  Grid.set data start 1;

  let valid_steps pos =
    List.filter
      (fun p -> Grid.at data pos + 1 >= Grid.at data p)
      (Grid.neighbours data pos)
  in

  let module Solver = Dijkstra (Grid.PosComp) in
  List.fold_left
    (fun min_val pos ->
      log (fun _ -> print_char '.');
      if Grid.at data pos = 1 then
        let dist_from_here = Solver.find_shortest pos target valid_steps in
        if dist_from_here < min_val then dist_from_here else min_val
      else min_val)
    Solver.inf (Grid.all_pos data)

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
