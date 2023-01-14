(*
   Day 22
*)

open Common

type tpos = { r : int; c : int }

let ( +! ) a b = { r = a.r + b.r; c = a.c + b.c }
let ( -! ) a b = { r = a.r - b.r; c = a.c - b.c }

type tinstr = Move of int | Clockwise | CounterClock

class ['a, 'b] grid (init_shadow : 'b) (init_contents : 'a list list) =
  let height = List.length init_contents in
  let width = List.length (List.hd init_contents) in

  object (self)
    (* Row 0 is at the top *)
    val data = Array.of_list (List.map Array.of_list init_contents)
    val shadow_data = Array.init height (fun _ -> Array.make width init_shadow)
    method height = height
    method width = width
    method set_shadow pos value = shadow_data.(pos.r).(pos.c) <- value
    method at pos = data.(pos.r).(pos.c)
    method shadow_at pos = shadow_data.(pos.r).(pos.c)

    (* Do pos +! incr until a pos that is not equal to v and return that pos*)
    method find_neq pos pos_incr (v : 'a) =
      let rec find pos =
        if self#at pos <> v then pos else find (pos +! pos_incr)
      in
      find pos
  end

type tstate = { pos : tpos; dir : tpos }

let read_data ic =
  let string_to_list str = String.fold_right List.cons str [] in
  let rec grid_reader () =
    match input_line ic with
    | "" -> []
    | row -> string_to_list row :: grid_reader ()
  in
  let grid_list = grid_reader () in
  let char_to_int c = int_of_char c - 48 in
  let rec parse_instrs = function
    | ('0' .. '9' as n1) :: ('0' .. '9' as n2) :: rest ->
        Move ((char_to_int n1 * 10) + char_to_int n2) :: parse_instrs rest
    | ('0' .. '9' as n) :: rest -> Move (char_to_int n) :: parse_instrs rest
    | 'R' :: rest -> Clockwise :: parse_instrs rest
    | 'L' :: rest -> CounterClock :: parse_instrs rest
    | [] -> []
    | _ -> assert false
  in
  (grid_list, parse_instrs (string_to_list (input_line ic)))

let make_grid rowlist =
  (* Add an empty border around input data. *)
  (* This works out well, since the solution expects top-left coordinate to be 1, and grid uses 0 *)
  let width = List.length (List.hd rowlist) in
  let rowlist =
    let empty_row = List.of_seq (Seq.init (width + 2) (fun _ -> ' ')) in
    (empty_row :: List.map (fun row -> (' ' :: row) @ [ ' ' ]) rowlist)
    @ [ empty_row ]
  in
  let grid = new grid None rowlist in

  (* Put wormholes at horizontal bounds of data *)
  for row = 1 to grid#height - 2 do
    let left = grid#find_neq { r = row; c = 0 } { r = 0; c = 1 } ' ' in
    let right =
      grid#find_neq { r = row; c = grid#width - 1 } { r = 0; c = -1 } ' '
    in
    grid#set_shadow { r = row; c = left.c - 1 } (Some right);
    grid#set_shadow { r = row; c = right.c + 1 } (Some left)
  done;

  (* And on the vertical *)
  for col = 1 to grid#width - 2 do
    let top = grid#find_neq { r = 0; c = col } { r = 1; c = 0 } ' ' in
    let bot =
      grid#find_neq { r = grid#height - 1; c = col } { r = -1; c = 0 } ' '
    in
    grid#set_shadow { r = top.r - 1; c = col } (Some bot);
    grid#set_shadow { r = bot.r + 1; c = col } (Some top)
  done;

  grid

(* Clockwise rotate *)
let rot = function
  | { r = 0; c = 1 } -> { r = 1; c = 0 }
  | { r = 1; c = 0 } -> { r = 0; c = -1 }
  | { r = 0; c = -1 } -> { r = -1; c = 0 }
  | { r = -1; c = 0 } -> { r = 0; c = 1 }
  | _ -> assert false

(* Counter-clockwise rotate *)
let crot dir =
  (* :) *)
  rot (rot (rot dir))

let run_cmd (grid : (char, tpos option) grid) state cmd =
  let rec step state _ =
    let npos = state.pos +! state.dir in
    match grid#at npos with
    | '.' -> { state with pos = npos }
    | '#' -> state
    | ' ' ->
        (* Hack *)
        let npos =
          (match grid#shadow_at npos with
          | Some pos -> pos
          | None -> assert false)
          -! state.dir
        in
        step { state with pos = npos } 0
    | _ -> assert false
  in

  match cmd with
  | Clockwise -> { state with dir = rot state.dir }
  | CounterClock -> { state with dir = crot state.dir }
  | Move n -> Seq.fold_left step state (Seq.init n Fun.id)

let parta (rowlist, cmds) =
  let grid = make_grid rowlist in
  let start_pos = grid#find_neq { r = 1; c = 0 } { r = 0; c = 1 } ' ' in
  let pos = { pos = start_pos; dir = { r = 0; c = 1 } } in
  let final = List.fold_left (run_cmd grid) pos cmds in
  let face_num =
    match final.dir with
    | { r = 0; c = 1 } -> 0
    | { r = 1; c = 0 } -> 2
    | { r = 0; c = -1 } -> 3
    | { r = -1; c = 0 } -> 4
    | _ -> assert false
  in
  (final.pos.r * 1000) + (final.pos.c * 4) + face_num

let partb _data = 0

(*
   Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
