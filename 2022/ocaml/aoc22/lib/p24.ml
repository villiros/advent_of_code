(*
   Day 24
*)
open Common

type tpos = { r : int; c : int }

let ( +! ) a b = { r = a.r + b.r; c = a.c + b.c }
let mpos r c = { r; c }

type tvortex = Up | Down | Left | Right

let all_vortexes = [ Up; Down; Left; Right ]

(* Single cell containing vortexes (bitfield) and saying where they go on the next step. *)
class cell (initial : tvortex option) =
  let to_bit = function Up -> 1 | Down -> 2 | Left -> 4 | Right -> 8 in

  let dir_to_dpos = function
    | Up -> { r = ~-1; c = 0 }
    | Down -> { r = 1; c = 0 }
    | Left -> { r = 0; c = ~-1 }
    | Right -> { r = 0; c = 1 }
  in

  let bits_to_char = function
    | 0 -> '.'
    | 1 -> '^'
    | 2 -> 'v'
    | 4 -> '<'
    | 8 -> '>'
    | _ -> '@'
  in

  object (self)
    val mutable data = match initial with Some x -> to_bit x | None -> 0

    method is_empty = data = 0
    method add v = data <- data lor to_bit v

    method get =
      List.fold_left
        (fun acc dir -> if data land to_bit dir <> 0 then dir :: acc else acc)
        [] all_vortexes

    (* Returns a list of (tvortex, movement vector) for where contents goes. *)
    method next =
      let vorts = self#get in
      List.map (fun v -> (v, dir_to_dpos v)) vorts

    method to_char = bits_to_char data
  end

(* Grid of vortexes. *)
class grid width height =
  object (self)
    val data =
      Array.init height (fun _ -> Array.init width (fun _ -> new cell None))

    method get pos = data.(pos.r).(pos.c)

    method get_opt pos =
      if pos.r >= 0 && pos.r < height && pos.c >= 0 && pos.c < width then
        Some (self#get pos)
      else None

    method add pos vort = data.(pos.r).(pos.c)#add vort

    (* Setup a row from input string *)
    method set_str_row row s =
      let cell col c =
        let adder = self#add (mpos row col) in
        match c with
        | '.' -> ()
        | '<' -> adder Left
        | '>' -> adder Right
        | '^' -> adder Up
        | 'v' -> adder Down
        | _ -> assert false
      in
      String.iteri cell s

    (* Return a list of vectors with valid moves into this grid from a previous minute.
       pos is where the party is at the previous minute. *)
    method get_moves pos =
      let moves = [ mpos 0 0; mpos ~-1 0; mpos 1 0; mpos 0 ~-1; mpos 0 1 ] in
      List.fold_left
        (fun acc dp ->
          match self#get_opt (pos +! dp) with
          | None -> acc
          | Some x when x#is_empty -> (pos +! dp) :: acc
          | _ -> acc)
        [] moves

    (* Make a grid for the next minute, running the vortex simulation stuff. *)
    method make_next =
      let ngrid = new grid width height in
      for row = 0 to height - 1 do
        for col = 0 to width - 1 do
          let pos = mpos row col in
          let vorts = (self#get pos)#next in
          (* Add dpos and wrap around as needed. *)
          let addpos dpos =
            match pos +! dpos with
            | { r = -1; c } -> { r = height - 1; c }
            | { r; c } when r >= height -> { r = 0; c }
            | { r; c = -1 } -> { r; c = width - 1 }
            | { r; c } when c >= width -> { r; c = 0 }
            | p -> p
          in
          List.iter (fun (v, dpos) -> ngrid#add (addpos dpos) v) vorts
        done
      done;
      ngrid

    method print =
      if !verbose then
        for row = 0 to height - 1 do
          print_newline ();
          for col = 0 to width - 1 do
            print_char (self#get (mpos row col))#to_char
          done
        done
      else ()
  end

(* Generic Dijkstra graph search. Needs node identifier, and find_shortest needs a couple of functions
   to discover vertices and the destination.
   
   It does not do a full flood: goes until it reaches target.
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

  let find_shortest (start : t) (is_target: t -> bool) (neighbours : t -> t list) : int =
    let queue = ref Queue.empty in
    let dist = Hashtbl.create 100 in
    let result = ref None in

    let get_dist v =
      match Hashtbl.find_opt dist v with None -> inf | Some x -> x
    in

    Hashtbl.replace dist start 0;
    queue := Queue.add (0, start) !queue;

    while Option.is_none !result && not (Queue.is_empty !queue) do
      let udist, u = Queue.min_elt !queue in
      queue := Queue.remove (udist, u) !queue;
      if is_target u then result := Some (get_dist u)
      else
        let alt = get_dist u + 1 in
        List.iter
          (fun v ->
            let old_dist = get_dist v in
            if alt < old_dist then (
              queue := Queue.remove (old_dist, v) !queue;
              queue := Queue.add (alt, v) !queue;
              Hashtbl.replace dist v alt))
          (neighbours u)
    done;
    Option.get !result
end

let read_data ic =
  let rem_walls s = String.sub s 1 (String.length s - 2) in
  ignore (input_line ic);
  let rec reader acc =
    match input_line ic with
    | exception End_of_file -> acc
    | s -> reader (rem_walls s :: acc)
  in
  (* Also drop the last row here *)
  match reader [] with _ :: rest -> List.rev rest | _ -> assert false

let make_grid_from_data (data : string list) =
  let width = String.length (List.hd data) in
  let height = List.length data in
  let endp = { r = height - 1; c = width - 1 } in

  let grid = new grid width height in
  List.iteri grid#set_str_row data;

  (grid, endp)

(* Node_id for the Dijkstra search. Basically a tuple of time and position. *)
module TimeAndPos = struct
  type t = int * tpos

  let compare a b = compare a b
end

(* Maintains a list of grids (for every minute we consider) and, mainly, provides a neighbours
   function to get valid moves from a TimeAndPos.
   
   The user of this starts and ends "outside" the grid; we need to know what those positions
   are so that we can add the "wait" move (the grid won't, as it's not a valid position for it.)
   *)
class grid_handler initial_grid startp endp =
  object
    val grids = Hashtbl.create 10

    method neighbours ((time, pos) : int * tpos) : (int * tpos) list =
      (* We want to know where we can move into in the next minute. *)
      let time = time + 1 in
      (* Search goes one step at a time only, so detect if we need to calculate a new minute. *)
      let grid =
        match Hashtbl.find_opt grids time with
        | Some x -> x
        | None ->
            let prev = Hashtbl.find grids (time - 1) in
            let next = prev#make_next in
            Hashtbl.replace grids time next;
            next
      in
      let next_poses = grid#get_moves pos in
      let result = List.map (fun p -> (time, p)) next_poses in
      (* Start & end pos are outside the grid, so need to add in "wait" by hand. *)
      if pos = startp || pos = endp then (time, pos) :: result else result

    initializer Hashtbl.replace grids 0 initial_grid
  end

let parta data =
  let grid, endp = make_grid_from_data data in
  let startp = { r = ~-1; c = 0 } in
  let handler = new grid_handler grid startp endp in

  let module Solver = Dijkstra (TimeAndPos) in
  Solver.find_shortest
    (0, { r = ~-1; c = 0 })
    (fun (_, p) -> p = endp)
    handler#neighbours
  + 1

let partb data =
  (* Endp is in the grid, 1 above the actual target *)
  let grid, endp = make_grid_from_data data in
  let target = endp +! { r = 1; c = 0 } in
  let start = { r = ~-1; c = 0 } in
  (* Position before we are about to move to start, 1 below the start *)
  let startp = start +! { r = 1; c = 0 } in
  let handler = new grid_handler grid start target in
  let module Solver = Dijkstra (TimeAndPos) in
  (* Some notes: becaue the find ends at endp (within the grid), the actual time is +1 . Same for the other ones.
     Also, we are skipping the "move to the start/end" step, but the neighbours function expects that it's asked
     one step at a time. So just call it to prime things. *)
  let there =
    Solver.find_shortest (0, start) (fun (_, p) -> p = endp) handler#neighbours
    + 1
  in
  let back =
    Solver.find_shortest (there, target)
      (fun (_, p) -> p = startp)
      handler#neighbours
    + 1
  in
  ignore (handler#neighbours (there + back - 1, start));
  let and_back_again =
    Solver.find_shortest
      (there + back, start)
      (fun (_, p) -> p = endp)
      handler#neighbours
    + 1
  in
  logv (there, back, and_back_again);
  there + back + and_back_again

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
