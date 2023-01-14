(*
Day 23
*)

open Common

(* Apply fn to all la * lb *)
let rec product_fold fn init la lb =
  match la with
  | [] -> init
  | hd :: tl ->
      let acc2 = List.fold_left (fun acc lbv -> fn acc (hd, lbv)) init lb in
      product_fold fn acc2 tl lb

type tpos = { r : int; c : int }

let ( +! ) pos1 pos2 = { r = pos1.r + pos2.r; c = pos1.c + pos2.c }
let area (pos1, pos2) = abs (pos2.r - pos1.r) * abs (pos2.c - pos1.c)

type tstep = { to_check : tpos list; move : tpos }

let make_steps =
  [
    {
      to_check =
        [ { r = ~-1; c = ~-1 }; { r = ~-1; c = 0 }; { r = ~-1; c = 1 } ];
      move = { r = ~-1; c = 0 };
    };
    {
      to_check = [ { r = 1; c = ~-1 }; { r = 1; c = 0 }; { r = 1; c = 1 } ];
      move = { r = 1; c = 0 };
    };
    {
      to_check =
        [ { r = ~-1; c = ~-1 }; { r = 0; c = ~-1 }; { r = 1; c = ~-1 } ];
      move = { r = 0; c = ~-1 };
    };
    {
      to_check = [ { r = ~-1; c = 1 }; { r = 0; c = 1 }; { r = 1; c = 1 } ];
      move = { r = 0; c = 1 };
    };
  ]

let adjacents =
  product_fold
    (fun acc -> function 0, 0 -> acc | r, c -> { r; c } :: acc)
    [] [ ~-1; 0; 1 ] [ ~-1; 0; 1 ]

let rot list = List.tl list @ [ List.hd list ]

class particle_set =
  object (self)
    val data : (tpos, int) Hashtbl.t = Hashtbl.create 10000
    method is_empty = Hashtbl.length data = 0

    method add (pos : tpos) =
      match Hashtbl.find_opt data pos with
      | None -> Hashtbl.add data pos 1
      | Some c -> Hashtbl.add data pos (c + 1)

    method count pos =
      match Hashtbl.find_opt data pos with None -> 0 | Some x -> x

    method has pos = self#count pos > 0

    method num_pos =
      Hashtbl.fold (fun _ c acc -> if c > 0 then acc + 1 else acc) data 0

    method bounds =
      let min_row =
        Hashtbl.fold
          (fun pos c acc -> if c > 0 then min acc pos.r else acc)
          data 1000000000000
      in
      let max_row =
        Hashtbl.fold
          (fun pos c acc -> if c > 0 then max acc pos.r else acc)
          data ~-1000000000000
      in
      let min_col =
        Hashtbl.fold
          (fun pos c acc -> if c > 0 then min acc pos.c else acc)
          data 1000000000000
      in
      let max_col =
        Hashtbl.fold
          (fun pos c acc -> if c > 0 then max acc pos.c else acc)
          data ~-1000000000000
      in
      ({ r = min_row; c = min_col }, { r = max_row + 1; c = max_col + 1 })

    method iter fn =
      Hashtbl.iter (fun pos c -> if c > 0 then fn pos c else ()) data

    method print =
      if !verbose then
        let bmin, bmax = self#bounds in
        for r = bmin.r to bmax.r do
          for c = bmin.c to bmax.c do
            if self#has { r; c } then print_char '#' else print_char '.'
          done;
          print_newline ()
        done
  end

let make_particle_set_from_input data =
  let res = new particle_set in
  List.iteri
    (fun r row ->
      List.iteri
        (fun c s ->
          match s with '.' -> () | '#' -> res#add { r; c } | _ -> assert false)
        row)
    data;
  res

let rec read_data ic =
  let rec read_row s pos =
    if pos = String.length s then [] else s.[pos] :: read_row s (pos + 1)
  in
  match input_line ic with
  | exception End_of_file -> []
  | s -> read_row s 0 :: read_data ic

let run_round parts moves =
  let propmoves = new particle_set in
  let find_move pos =
    let empty_to dpos = not (parts#has (pos +! dpos)) in
    if List.for_all empty_to adjacents then None
    else
      let can_move { to_check; _ } = List.for_all empty_to to_check in
      List.find_opt can_move moves
  in
  let calc_proposed pos _count =
    match find_move pos with
    | None -> ()
    | Some { move; _ } -> propmoves#add (pos +! move)
  in
  parts#iter calc_proposed;

  if propmoves#is_empty then (false, parts, rot moves)
  else
    let next_parts = new particle_set in
    let do_moves pos _count =
      match find_move pos with
      | None -> next_parts#add pos
      | Some { move; _ } when propmoves#count (pos +! move) = 1 ->
          next_parts#add (pos +! move)
      | Some { move; _ } ->
          assert (propmoves#count (pos +! move) > 0);
          next_parts#add pos
    in
    parts#iter do_moves;
    (true, next_parts, rot moves)

let parta data =
  let parts = ref (make_particle_set_from_input data) in
  let moves = ref make_steps in
  for _i = 1 to 10 do
    let _had_moves, nparts, nmoves = run_round !parts !moves in
    parts := nparts;
    moves := nmoves;
    logv ("after round ", _i);
    !parts#print
  done;

  logv !parts#bounds;
  area !parts#bounds - !parts#num_pos

let partb data =
  let parts = ref (make_particle_set_from_input data) in
  let moves = ref make_steps in

  let res =
    Seq.find
      (fun _ ->
        let had_moves, nparts, nmoves = run_round !parts !moves in
        if not had_moves then true
        else (
          parts := nparts;
          moves := nmoves;
          false))
      (Seq.ints 1)
  in
  match res with Some x -> x | None -> assert false

(*
Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
