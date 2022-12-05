(*
 * Day 5
 *)

open Common

type command = { num_move : int; source : int; dest : int }

module Ship = struct
  type tcrate = char
  type tstack = tcrate list

  open Array

  class t =
    let max_stacks = 10 in
    object
      val data = make max_stacks []
      method put i (crates : tstack) = data.(i) <- crates @ data.(i)
      method put_bottom i (crate : tcrate) = data.(i) <- data.(i) @ [ crate ]

      method move part { num_move; source; dest } =
        let rec split_at = function
          | 0, rest -> ([], rest)
          | n, a :: rest ->
              let f, s = split_at (n - 1, rest) in
              (a :: f, s)
          | _ -> assert false
        in
        let load, new_source = split_at (num_move, data.(source)) in
        let load2 = match part with PartA -> List.rev load | PartB -> load in
        data.(dest) <- load2 @ data.(dest);
        data.(source) <- new_source

      method tops =
        let rec combine = function
          | i when i = max_stacks -> ""
          | i ->
              let cc =
                match data.(i) with h :: _ -> String.make 1 h | _ -> ""
              in
              cc ^ combine (i + 1)
        in
        combine 0
    end
end

let read_ship ic =
  let ship = new Ship.t in
  let rec read_manifest () =
    match input_line ic with
    | exception End_of_file -> assert false
    | ln when (String.starts_with ~prefix:" 1" ln) ->
      assert ((input_line ic) = "")
    | ln ->
        let rec rline s row =
          let ln_i = (row - 1) * 4 in
          match String.sub ln ln_i 3 with
          | exception Invalid_argument _ -> ()
          | "   " -> rline s (row + 1)
          | x ->
              ship#put_bottom row x.[1];
              rline s (row + 1)
        in
        rline ship 1;
        read_manifest ()
  in
  read_manifest ();
  ship

let read_commands ic =
  let rec read ic acc =
    match input_line ic with
    | exception End_of_file -> acc
    | s -> (
        match String.split_on_char ' ' s with
        | [ "move"; nums; "from"; srcs; "to"; dsts ] ->
            read ic
              ({
                 num_move = int_of_string nums;
                 source = int_of_string srcs;
                 dest = int_of_string dsts;
               }
              :: acc)
        | _other -> assert false)
  in
  List.rev (read ic [])

let read_data ic =
  let ship = read_ship ic in
  (ship, read_commands ic)

let parta ((ship : Ship.t), (cmds : command list)) =
  List.iter (ship#move PartA) cmds;
  ship#tops

let partb ((ship : Ship.t), (cmds : command list)) =
  List.iter (ship#move PartB) cmds;
  ship#tops

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Str (parta (read_data ic))
  | PartB -> Str (partb (read_data ic))
