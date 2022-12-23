(*
 * Day 13
 *)

open Common

let min a b = if a < b then a else b
let max a b = if a > b then a else b

(* Signature for elements stored in a grid *)
module type GridEl = sig
  type t

  val initial : t
  val to_str : t -> string
end

(* Rectangular grid of elements. *)
module GenGrid (ElType : GridEl) : sig
  (* Publicly-visible position *)
  type tpos = { r : int; c : int }

  (* Position constructor *)
  val pos : int -> int -> tpos
  val ( +! ) : tpos -> tpos -> tpos
  val ( -! ) : tpos -> tpos -> tpos

  (* Grid. Takes the top-left and bottom-right position. It's actually flipped: row in bottom-right is positive. *)
  class grid :
    tpos
    -> tpos
    -> object
         (* Get at position. Returns None if out of bounds. *)
         method at : tpos -> ElType.t option
         method put : tpos -> ElType.t -> unit

         (* Fill horizontal/vertical line from start to end inclusive *)
         method fill_line : tpos -> tpos -> ElType.t -> unit
         method print : unit
       end
end = struct
  type tpos = { r : int; c : int } (* row column *)

  let pos r c = { r; c }

  (* Type used inside grid to differentiate positions in the local coordinates *)
  type tpos_transposed = Local of tpos

  let ( +! ) { r = r1; c = c1 } { r = r2; c = c2 } =
    { r = r1 + r2; c = c1 + c2 }

  let ( -! ) { r = r1; c = c1 } { r = r2; c = c2 } =
    { r = r1 - r2; c = c1 - c2 }

  let magn { r; c } =
    {
      r = (match r with 0 -> 0 | _ when r < 0 -> -1 | _ -> 1);
      c = (match c with 0 -> 0 | _ when c < 0 -> -1 | _ -> 1);
    }

  (* The grid uses a local coordinate system where top_left is at (0, 0) *)
  class grid (top_left : tpos) (bot_right : tpos) =
    let unwrap (Local pos) = pos in
    (* Size of the grid we'll actually need *)
    let psize = bot_right -! top_left +! { r = 1; c = 1 } in

    object (self)
      val size = psize
      val data = Array.init psize.r (fun _ -> Array.make psize.c ElType.initial)
      method private transpose p = Local (p -! top_left)

      method private inside (Local p) =
        p.r >= 0 && p.r < size.r && p.c >= 0 && p.c < size.c

      method private get (Local p) = data.(p.r).(p.c)
      method private set (Local p) v = data.(p.r).(p.c) <- v

      method at pos =
        let p2 = self#transpose pos in
        if self#inside p2 then Some (self#get p2) else None

      method put pos v = self#set (self#transpose pos) v

      method fill_line pstart pend v =
        let step = magn (pend -! pstart) in
        let pstart2, pend2 = (self#transpose pstart, self#transpose pend) in
        assert (self#inside pstart2);
        assert (self#inside pend2);
        assert (step.r = 0 || step.c = 0);

        let p = ref pstart2 in
        while !p <> pend2 do
          self#set !p v;
          p := Local (unwrap !p +! step)
        done;
        (* End is inclusive. *)
        self#set !p v

      method print =
        if !verbose then (
          print_newline ();
          for r = 0 to size.r - 1 do
            for c = 0 to size.c - 1 do
              print_string (ElType.to_str (self#get (Local { r; c })))
            done;
            print_newline ()
          done)
    end
end

module Element = struct
  type t = Air | Rock | Sand

  let initial = Air
  let to_str = function Air -> "." | Rock -> "#" | Sand -> "o"
end

module Grid = GenGrid (Element)
open Grid

let sand_origin = pos 0 500

let read_data ic =
  let read_pos x =
    match String.split_on_char ',' x with
    | [ a; b ] -> pos (int_of_string b) (int_of_string a)
    | _ -> assert false
  in
  let rec read_segments prev = function
    | "->" :: next :: rest ->
        let nextp = read_pos next in
        (prev, nextp) :: read_segments nextp rest
    | [] -> []
    | _ -> assert false
  in
  let rec read_all () =
    match String.split_on_char ' ' (input_line ic) with
    | first :: rest -> read_segments (read_pos first) rest @ read_all ()
    | exception End_of_file -> []
    | _ -> assert false
  in
  read_all ()

let update_bounds (curmin, curmax) p =
  ( Grid.pos (min curmin.r p.r) (min curmin.c p.c),
    Grid.pos (max curmax.r p.r) (max curmax.c p.c) )

let get_bounds data =
  List.fold_left
    (fun acc (p1, p2) -> update_bounds (update_bounds acc p1) p2)
    (sand_origin, sand_origin) data

let sand_add grid =
  let fell_off_marker = Some (pos ~-1 ~-1) in
  let rec proc_sand p =
    let moves = [ p +! pos 1 0; p +! pos 1 ~-1; p +! pos 1 1 ] in
    let next_move =
      List.fold_left
        (fun acc p ->
          match (acc, grid#at p) with
          | None, Some Element.Air -> Some p
          | None, None -> fell_off_marker
          | _, _ -> acc)
        None moves
    in
    match next_move with
    | None ->
        grid#put p Element.Sand;
        1
    | p when p = fell_off_marker -> 0
    | Some p -> proc_sand p
  in
  let rec solver acc =
    match proc_sand sand_origin with
    | 0 -> acc
    | _ -> (
        match grid#at sand_origin with
        | Some Element.Sand -> acc + 1
        | _ -> solver (acc + 1))
  in
  solver 0

(* Common for both parts *)
let solve data =
  let top_left, bottom_right = get_bounds data in
  let grid = new grid top_left bottom_right in
  List.iter (fun (p1, p2) -> grid#fill_line p1 p2 Rock) data;
  let result = sand_add grid in
  grid#print;
  result

let parta data = solve data

let partb data =
  (* Add the floor*)
  let floor_line =
    let top_left, bottom_right = get_bounds data in
    let height = bottom_right.r - top_left.r in
    ( pos (bottom_right.r + 2) (sand_origin.c - height - 5),
      pos (bottom_right.r + 2) (sand_origin.c + height + 5) )
  in
  let data_with_floor = floor_line :: data in
  solve data_with_floor

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
