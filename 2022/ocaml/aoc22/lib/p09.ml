(*
 * Day 5
 *)

open Common

(* for i in n..0: f(i) *)
let rec repeat f = function
  | 0 -> ()
  | n ->
      f n;
      repeat f (n - 1)

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

(* Last element in a list *)
let rec last = function a :: [] -> a | _ :: x -> last x | [] -> assert false

type tdirection = Left | Right | Up | Down
type tcmd = { dir : tdirection; num : int }

module Position = struct
  type t = { x : int; y : int }

  let ( +! ) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
  let ( -! ) p1 p2 = { x = p1.x - p2.x; y = p1.y - p2.y }

  let compare { x = x1; y = y1 } { x = x2; y = y2 } =
    match compare x1 x2 with 0 -> compare y1 y2 | c -> c
end

module PosSet = Set.Make (Position)

(* A single link *)
class link =
  let open Position in
  object (self)
    val mutable head = { x = 0; y = 0 }
    val mutable tail = { x = 0; y = 0 }
    method get_tail () = tail

    val move_vector =
      function
      | Left -> { x = -1; y = 0 }
      | Right -> { x = 1; y = 0 }
      | Up -> { x = 0; y = 1 }
      | Down -> { x = 0; y = -1 }

    (* Moves the head in a direction and returns tail's move vector. *)
    method move dir = self#move2 (move_vector dir)

    (* Moves the head by a move vector and returns the tail's move vector. *)
    method move2 vector =
      head <- head +! vector;
      match head -! tail with
      | { x; y } when abs x <= 1 && abs y <= 1 -> { x = 0; y = 0 }
      | { x; y } ->
          let sign x = min 1 (max ~-1 x) in
          let tail_vec = { x = sign x; y = sign y } in
          tail <- tail +! tail_vec;
          assert (abs (head -! tail).x <= 1);
          assert (abs (head -! tail).y <= 1);
          tail_vec
  end

let rope length =
  object
    val links = List.fold_left (fun acc _ -> new link :: acc) [] (seq 1 length)

    method move dir =
      let first_vec = (List.hd links)#move dir in
      List.fold_left (fun vec el -> el#move2 vec) first_vec (List.tl links)

    method get_tail = (last links)#get_tail
  end

let read_data ic =
  let rec reader () =
    match input_line ic with
    | exception End_of_file -> []
    | ln ->
        let dir, nstr =
          match String.split_on_char ' ' ln with
          | [ "R"; n ] -> (Right, n)
          | [ "L"; n ] -> (Left, n)
          | [ "U"; n ] -> (Up, n)
          | [ "D"; n ] -> (Down, n)
          | _ -> assert false
        in
        { dir; num = int_of_string nstr } :: reader ()
  in
  reader ()

let parta data =
  let visited = ref PosSet.empty in
  let state = new link in
  let move dir _ =
    ignore (state#move dir);
    visited := PosSet.add (state#get_tail ()) !visited
  in
  List.iter (fun { dir; num } -> repeat (move dir) num) data;
  PosSet.cardinal !visited

let partb data =
  let visited = ref PosSet.empty in
  let state = rope 9 in
  let move dir _ =
    ignore (state#move dir);
    visited := PosSet.add (state#get_tail ()) !visited
  in
  List.iter (fun { dir; num } -> repeat (move dir) num) data;
  PosSet.cardinal !visited

(*
* Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
