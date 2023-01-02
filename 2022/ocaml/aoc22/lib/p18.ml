(*
 * Day 13
 *)

open Common

type tpos = { x : int; y : int; z : int }

let min refa b = if b < !refa then refa := b
let max refa b = if b > !refa then refa := b

let ( +! ) { x; y; z } { x = xb; y = yb; z = zb } =
  { x = x + xb; y = y + yb; z = z + zb }

let adjacents =
  let ds = [ -1; 1 ] in
  List.fold_left
    (fun acc d ->
      { x = d; y = 0; z = 0 } :: { x = 0; y = d; z = 0 }
      :: { x = 0; y = 0; z = d } :: acc)
    [] ds

let inside minpos maxpos pos =
  pos.x >= minpos.x && pos.x <= maxpos.x && pos.y >= minpos.y
  && pos.y <= maxpos.y && pos.z >= minpos.z && pos.z <= maxpos.z

let bounds vol =
  let minx, maxx, miny, maxy, minz, maxz =
    match Seq.uncons (Hashtbl.to_seq_keys vol) with
    | Some (pos, _) ->
        (ref pos.x, ref pos.x, ref pos.y, ref pos.y, ref pos.z, ref pos.z)
    | _ -> assert false
  in
  Hashtbl.iter
    (fun pos _ ->
      min minx pos.x;
      max maxx pos.x;
      min miny pos.y;
      max maxy pos.y;
      min minz pos.z;
      max maxz pos.z)
    vol;
  ( { x = !minx - 1; y = !miny - 1; z = !minz - 1 },
    { x = !maxx + 1; y = !maxy + 1; z = !maxz + 1 } )

let make_volume list =
  let result = Hashtbl.create 100 in
  List.iter (fun pos -> Hashtbl.add result pos true) list;
  result

let flood_from vol ipos minpos maxpos =
  assert (not (Hashtbl.mem vol ipos));
  let inside = inside minpos maxpos in

  let rec flood pos =
    let next =
      List.filter
        (fun pos -> inside pos && not (Hashtbl.mem vol pos))
        (List.map (( +! ) pos) adjacents)
    in
    List.iter (fun pos -> Hashtbl.add vol pos false) next;
    List.iter flood next
  in
  flood ipos

let rec read_data ic =
  match String.split_on_char ',' (input_line ic) with
  | exception End_of_file -> []
  | [ xs; ys; zs ] ->
      { x = int_of_string xs; y = int_of_string ys; z = int_of_string zs }
      :: read_data ic
  | _ -> assert false

let parta data =
  let vol = make_volume data in
  Hashtbl.fold
    (fun pos _ acc ->
      List.fold_left
        (fun acc dpos -> if Hashtbl.mem vol (pos +! dpos) then acc else 1 + acc)
        acc adjacents)
    vol 0

let partb data =
  let vol = make_volume data in
  let minpos, maxpos = bounds vol in
  flood_from vol minpos minpos maxpos;

  Hashtbl.fold
    (fun pos _ acc ->
      if Hashtbl.find vol pos then
        List.fold_left
          (fun acc dpos ->
            match Hashtbl.find_opt vol (pos +! dpos) with
            | Some false -> 1 + acc
            | _ -> acc)
          acc adjacents
      else acc)
    vol 0

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
