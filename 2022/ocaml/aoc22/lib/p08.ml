(*
 * Day 8
 *)
open Common

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

(* Supports square matrixes only *)
module Grid = struct
  type t = int Array.t Array.t

  type tpos = {r: int; c: int} (* row column *)

  type direction = Left | Right | Up | Down

  let directions = [Left; Right; Up; Down];;

  let dir_to_offset = function
  | Left ->  {r = 0; c = -1}
  | Right -> {r = 0; c = 1}
  | Up -> {r = -1; c = 0}
  | Down -> {r = 1; c = 0}

    let make max_index = Array.init max_index (fun _ -> Array.make max_index 0)

    let last_index g = (Array.length g) - 1

    let at g pos = g.(pos.r).(pos.c)
    let at2 g r c = g.(r).(c)
    let set g pos v = g.(pos.r).(pos.c) <- v

    let set_row grid row_data row_n =
      assert ((last_index grid) + 1 == (Array.length row_data));
      for j=0 to last_index grid do
        grid.(row_n).(j) <- row_data.(j)
      done;;

    let add a b = {r = a.r + b.r; c = a.c + b.c}
    let is_valid grid {r; c} =
      let size = last_index grid in r >= 0 && r <= size && c >= 0 && c <= size
    
    let rec fold_from grid direction (f: tpos -> 'a -> 'a) acc pos : 'a =
      if is_valid grid pos then
        fold_from grid direction f (f pos acc) (add pos (dir_to_offset direction))
      else
        acc
    
    let fold_all grid (f: tpos -> 'a -> 'a) acc =
        let edge = List.fold_right (fun i acc -> {r=i; c=0} :: acc) (seq 0 (last_index grid)) [] in
        let f = fold_from grid Right f in
        List.fold_left f acc edge;;
    
    let print_grid grid =
      let size = last_index grid in
      fold_all grid (fun pos _ -> 
        match pos with
        | {c; _} when c = size -> Format.printf "%3i\n" (at grid pos)
        | _ -> Format.printf "%3i " (at grid pos)) ()
    
    let edges grid =
      let size = last_index grid in
      let rec inner = function
      | i when i > size -> []
      | i -> {r = 0; c = i} :: {r = size; c = i} :: {r = i; c = 0} :: {r = i; c = size} :: inner (i+1)
    in
    inner 0;;
end;;

let ( << ) f g x = f (g x)

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

type tgrid = int Array.t Array.t
type tposition = { row : int; column : int }

let directions =
  [
    { row = 1; column = 0 };
    { row = 0; column = 1 };
    { row = -1; column = 0 };
    { row = 0; column = -1 };
  ]

let read_data ic : tgrid =
  let read_row () =
    match input_line ic with
    | ln ->
      (Array.init (String.length ln) (fun i -> int_of_char (String.get ln i) - 48))
  in
  let grid =
    let row = read_row () in
    let new_grid = Grid.make (Array.length row) in
    Grid.set_row new_grid row 0;
    new_grid
  in
  let rec reader row_num =
    match read_row () with
    | exception End_of_file -> grid
    | row -> Grid.set_row grid row row_num; reader (row_num + 1)
  in
    reader 1

let parta data =
  let seen = Grid.make ((Grid.last_index data) + 1) in
  let row_proc pos max_height =
    match Grid.at data pos with
    | n when n > max_height ->
      Grid.set seen pos 1;
      n
    | _ -> max_height
  in
  let edges = Grid.edges data in
  List.iter
    (fun pos ->
      (* Just do in all 4 directions from each *)
      ignore((List.iter (fun dir -> ignore(Grid.fold_from data dir row_proc (-1) pos)) Grid.directions)))
    edges;
  
  let counter pos acc = (Grid.at seen pos) + acc in
  Grid.fold_all seen counter 0

let partb data =
  let score_at pos =
    let distance_calc (pos: Grid.tpos) (from_height, distance) =
      match Grid.at data pos with
      | n when distance = -1 -> (n, 0)
      | n when n < from_height -> (from_height, distance + 1)
      | _ when from_height >= 0 -> (-1, distance + 1)
      | _ -> (-1, distance)
    in
    List.fold_left
      (fun acc dir ->
        let _, this_dist = Grid.fold_from data dir distance_calc (0, -1) pos in
        acc * this_dist)
      1
      Grid.directions
  in
    Grid.fold_all data (fun pos acc ->
       max (score_at pos) acc) 0;;

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
