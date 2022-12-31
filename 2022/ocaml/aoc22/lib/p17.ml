(*
 * Day 17
*)

open Common

let max a b = if a > b then a else b

(* Given a list, #pop returns elements from it
with a wrap around to the start. *)
class ['a] circular init_list = object (self)
  val data : 'a array = Array.of_list init_list
  val mutable pos = 0

  method get = data.(pos)
  method advance = pos <- (pos + 1) mod (Array.length data)
  method reset = pos <- 0

  method get_pos = pos

  method pop =
    let result = self#get in
    self#advance;
    result
end;;

class grid ?(height=0) ?(init_contents=[]) () =
  let empty = '.' in
  let rock = '#' in

  let height =
    match init_contents with
    | [] -> height
    | _ -> assert (height = 0); List.length init_contents
  in

  let width =
    match init_contents with
    | [] -> 7
    | row :: _ -> String.length row
  in

  object (self)
    (* Row 0 is at the bottom *)
    val data = Array.init height (fun _ -> Array.make width empty)
    (* Lowest row that is empty *)
    val mutable first_empty_row = 0

    method height = height
    method width = width
    method first_empty_row = first_empty_row

    method at row col = data.(row).(col)

    method in_bounds row col =
      row >= 0 && row < height && col >= 0 && col < width

    (* Returns true if other grid can be placed at (row, col) without colliding (or being out of bounds). *)
    method intersect (other:grid) (row:int) (col:int) =
      try (
        for r=0 to (other#height - 1) do
          for c=0 to (other#width - 1) do
            if (other#at r c) = rock then (
              if not (self#in_bounds (row + r) (col + c)) then
                raise Not_found;
              if data.(row+r).(col+c) <> empty then
                raise Not_found
            )
          done
        done;
        true)
      with
        Not_found -> false
    
    method place (other:grid) (row:int) (col:int) =
      assert (self#intersect other row col);
      for r=0 to other#height - 1 do
        for c=0 to other#width - 1 do
          if (other#at r c) = rock then (
            data.(row+r).(col+c) <- rock;
            if (row + r + 1) > first_empty_row then first_empty_row <- row + r + 1
          )
        done
      done
    
    (* Returns a list of ints representing the state of some top rows of the grid.
       The rows are chosen such that there's a rock in every column. *)
    method top_lines_state =
      assert (width = 7);
      let row_to_int row =
        Array.fold_left
          (fun acc c ->
            (Int.shift_left acc 1) + (if c = rock then 1 else 0))
            0 row
      in
      let result = ref [] in
      let cols_covered = ref 0 in
      try
        for i=(first_empty_row - 1) downto 0 do
          result := (row_to_int data.(i)) :: !result;
          cols_covered := (Int.logor !cols_covered (row_to_int data.(i)));
          if !cols_covered = 127 then raise Exit
        done;
        !result
      with Exit ->
        !result
    
    method print ?(limit=5) () =
      if !verbose then
        for i=first_empty_row downto (max 0 (first_empty_row - limit)) do
          Array.iter print_char data.(i);
          print_newline ()
        done

    initializer
      List.iteri 
        (fun rowi rowdata ->
          assert ((String.length rowdata) = width);
          String.iteri (fun col c -> data.(rowi).(col) <- c) rowdata)
        (List.rev init_contents);
      if init_contents <> [] then
        (* This is nonsensical. It's not a valid operation to get empty row on pre-filled grids. *)
        first_empty_row <- height
        
end

let pieces () =
  let plist = [
    new grid ~init_contents:["####"] ();
    new grid ~init_contents:[".#."; "###"; ".#."] ();
    new grid ~init_contents:["..#"; "..#"; "###"] ();
    new grid ~init_contents:["#"; "#"; "#"; "#"] ();
    new grid ~init_contents:["##"; "##"] ()
  ] in
  new circular plist

type tdir = Right | Left

let read_data ic =
  let move_list =
    String.fold_right
      (fun x acc ->
        match x with
        | '>' -> Right :: acc
        | '<' -> Left :: acc
        | _ -> assert false)
      (input_line ic)
      []
  in
  new circular move_list

(* Move one piece until it rests. *)
let rec process_piece grid moves piece row col =
  assert (grid#intersect piece row col);
  let new_col = 
    match moves#pop with
    | Left -> col - 1
    | Right -> col + 1
  in
  let new_col =
    if grid#intersect piece row new_col then new_col else col
  in
  if grid#intersect piece (row - 1) new_col then
    process_piece grid moves piece (row - 1) new_col
  else
    grid#place piece row new_col

(* Run simulation until a period is detected. Its details are returned. *)
let find_interval (grid:grid) (data:tdir circular) pieces =
  let memo = Hashtbl.create 100 in
  let rec finder i =
    let state_before = (grid#top_lines_state, pieces#get_pos, data#get_pos) in
    process_piece grid data pieces#pop (grid#first_empty_row + 3) 2;
    let state_after = (grid#top_lines_state, pieces#get_pos, data#get_pos) in

    match Hashtbl.find memo (state_before, state_after) with
    | exception Not_found -> (
      Hashtbl.add memo (state_before, state_after) (i, grid#first_empty_row);
      finder (i + 1))
    | (prev_iter, prev_empty_row) ->
      (i, i - prev_iter, grid#first_empty_row - prev_empty_row)
  in
  let prefix, interval, interval_rows = finder 1 in
  (prefix, interval, interval_rows)

let solve data num_to_do =
  let grid = new grid ~height:30000 () in
  let pieces = pieces () in
  (* Simulate until a period is seen. *)
  let prefix, interval, interval_rows = find_interval grid data pieces in
  logv ("find_interval",
        "prefix", prefix, "interval", interval, "interval_rows", interval_rows,
        "rows so far", grid#first_empty_row);

  (* In part A period might be longer than target. Just re-simulate normally then. *)
  if prefix < num_to_do then (
    (* We can skip a multiple of interval, just the remainder to do *)
    (* prefix is how many steps were done by find_interval already *)
    let remaining = (num_to_do - prefix) mod interval in
    for _=1 to remaining do
      process_piece grid data pieces#pop (grid#first_empty_row + 3) 2
    done;

    logv ("prefix", prefix, "interval", interval, "interval_rows", interval_rows, "remaining", remaining, "intervals_skipped", ((num_to_do - prefix) / interval));
    logv ("first_empty_row", grid#first_empty_row);
    grid#first_empty_row + (interval_rows * (((num_to_do - prefix) / interval)))
  )
  else (
    let grid = new grid ~height:30000 () in
    pieces#reset;
    data#reset;
    for _=1 to num_to_do do
      process_piece grid data pieces#pop (grid#first_empty_row + 3) 2
    done;
  
    grid#first_empty_row
  )
  

let parta data =
  solve data 2022

let partb data =
  solve data 1000000000000

(*
 * Runner
 *)

let solve part ic =
match part with
| PartA -> Int (parta (read_data ic))
| PartB -> Int (partb (read_data ic))