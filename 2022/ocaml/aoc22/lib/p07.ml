(*
 * Day 7
 *)

open Common

(* Input commands *)
type input_entry =
  | Cd of { name : string }
  | Ls
  | ODir of { name : string }
  | OFile of { name : string; size : int }

(* Directory tree. *)
type tdir =
  | File of { name : string; size : int }
  | Dir of {
      name : string;
      mutable total_size : int;
      mutable contents : tdir list;
    }

let rec loader (ploc : tdir list) cmds =
  match ploc with
  | Dir { name = "/"; _ } :: _ when List.hd cmds = Cd { name = "/" } ->
      (* Thankfully input starts with cd / , otherwise we'd need to track the root *)
      loader ploc (List.tl cmds)
  | Dir loc :: prev -> (
      let locate name =
        match
          List.find
            (function File _ -> false | Dir dir -> dir.name = name)
            loc.contents
        with
        | exception Not_found ->
            let newdir = Dir { name; total_size = 0; contents = [] } in
            loc.contents <- newdir :: loc.contents;
            newdir
        | Dir _ as n -> n
        | File _ -> assert false
      in
      match cmds with
      | [] -> ()
      | Cd { name = ".."; _ } :: rest -> loader prev rest
      | Cd { name; _ } :: rest -> loader (locate name :: ploc) rest
      | Ls :: rest -> loader ploc rest
      | ODir { name } :: rest ->
          ignore (locate name);
          loader ploc rest
      | OFile { name; size } :: rest ->
          loc.contents <- File { name; size } :: loc.contents;
          loader ploc rest)
  | _ -> assert false

type tvisitor_op = Item | Leave
type 'a tvisitor = tvisitor_op -> 'a -> tdir -> 'a

let rec dir_traverse (visitor : 'a tvisitor) (acc : 'a) (dir : tdir) =
  match dir with
  | File _ as f -> visitor Item acc f
  | Dir { contents; _ } ->
      let acc2 = visitor Item acc dir in
      let new_acc = List.fold_left (dir_traverse visitor) acc2 contents in
      visitor Leave new_acc dir

let calc_sizes (dir : tdir) =
  let visitor op acc (item : tdir) =
    match (op, item, acc) with
    | Item, File { size; _ }, sh :: sr -> (sh + size) :: sr
    | Item, Dir _, _ -> 0 :: acc
    | Leave, Dir dir, sh :: sr :: srr ->
        dir.total_size <- sh;
        (sr + sh) :: srr
    | Leave, Dir { name = "/"; _ }, _ :: [] ->
        (* Finished *)
        []
    | _else -> assert false
  in
  match (dir, dir_traverse visitor [ 0 ] dir) with
  | Dir dir, [ res ] ->
      dir.total_size <- res;
      res
  | _ -> assert false

let sub s index = String.sub s index (String.length s - index)

let read_data ic =
  let rec reader ic =
    match input_line ic with
    | exception End_of_file -> []
    | ln -> (
        match String.sub ln 0 4 with
        | "$ cd" -> Cd { name = sub ln 5 } :: reader ic
        | "$ ls" -> Ls :: reader ic
        | "dir " -> ODir { name = sub ln 4 } :: reader ic
        | _ -> (
            assert ('0' <= ln.[0] && ln.[0] <= '9');
            match String.split_on_char ' ' ln with
            | size_str :: name :: _ ->
                OFile { name; size = int_of_string size_str } :: reader ic
            | _ -> assert false))
  in
  reader ic

let parta data =
  let dir = Dir { name = "/"; total_size = 0; contents = [] } in
  loader [ dir ] data;
  ignore (calc_sizes dir);

  let sum_adder op acc dir =
    match (op, acc, dir) with
    | Leave, acc, Dir { total_size; _ } when total_size <= 100000 ->
        acc + total_size
    | _ -> acc
  in
  dir_traverse sum_adder 0 dir

let partb data =
  let dir = Dir { name = "/"; total_size = 0; contents = [] } in
  loader [ dir ] data;

  let used_space = calc_sizes dir in
  let to_delete = 30000000 - (70000000 - used_space) in

  let target_selector op acc dir =
    match (op, acc, dir) with
    | Leave, best_size, Dir { total_size; _ }
      when total_size >= to_delete && total_size < best_size ->
        total_size
    | _ -> acc
  in
  dir_traverse target_selector 70000000 dir

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
