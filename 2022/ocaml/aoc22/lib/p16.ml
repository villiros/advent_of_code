(*
   Day 16
*)

open Common

let max a b = if a > b then a else b

(* Apply fn to all la * lb *)
let rec product_iter fn la lb =
  match la with
  | [] -> ()
  | hd :: tl ->
      List.iter (fn hd) lb;
      product_iter fn tl lb

(* Like product la la, but with symmetric entries skipped: (1,2) and (2,1) are symmetric*)
let rec asym_product_iter fn la =
  match la with
  | [] -> ()
  | hd :: tl ->
      product_iter fn [ hd ] la;
      asym_product_iter fn tl

type tnode_idx = int

type tnode = {
  idx : tnode_idx;
  rate : int;
  mutable conn : tnode_idx list;
  mutable is_open : bool;
}

let parse_regexp =
  Str.regexp
    {|Valve \(..\) has flow rate=\([0-9]+\); tunnels? leads? to valves? \(.+\)|}

let rec solve data ~rem_time ~cur_pres ~best_ref ~closed_v_sum ~open_v_sum
    ~node_idx ~prev_visited =
  if Random.int 100000 = 0 then logv (rem_time, !best_ref);
  let node = data.(node_idx) in

  best_ref := max !best_ref (cur_pres + (rem_time * open_v_sum));

  (* If there's a closed valve here, we need a minute to open and a minute to act *)
  (* Otherwise, we'd need 3 minutes: 1 to go elsewhere, and then open a valve there*)
  if (not node.is_open) && node.rate > 0 && rem_time < 2 then ()
  else if (node.is_open || node.rate == 0) && rem_time < 3 then ()
  else if closed_v_sum = 0 then ()
  else if
    cur_pres + (rem_time * open_v_sum) + ((rem_time - 1) * closed_v_sum)
    <= !best_ref
  then (* No way we can beat the best result already *)
    ()
  else (
    (* Try opening the valve first *)
    if (not node.is_open) && node.rate > 0 then (
      node.is_open <- true;
      let next_cur_pres = cur_pres + open_v_sum in
      solve data ~rem_time:(rem_time - 1) ~best_ref ~cur_pres:next_cur_pres
        ~closed_v_sum:(closed_v_sum - node.rate)
        ~open_v_sum:(open_v_sum + node.rate) ~node_idx ~prev_visited:node_idx;
      node.is_open <- false);

    List.iter
      (fun idx ->
        (* If we just came from idx, don't got back there *)
        if idx <> prev_visited then
          let next_cur_pres = cur_pres + open_v_sum in
          solve data ~rem_time:(rem_time - 1) ~best_ref ~cur_pres:next_cur_pres
            ~closed_v_sum ~open_v_sum ~node_idx:idx ~prev_visited:node_idx)
      node.conn)

(* Slow approach: just step both self and elephant through all possible combinations. *)
let rec solve_b data ~rem_time ~cur_pres ~best_ref ~closed_v_sum ~open_v_sum
    ~my_idx ~my_prev_visited ~el_idx ~el_prev_visited =
  if Random.int 10000000 = 0 then logv (rem_time, !best_ref);

  let my_node = data.(my_idx) in
  let el_node = data.(el_idx) in

  (* We might decide to stop *)
  best_ref := max !best_ref (cur_pres + (rem_time * open_v_sum));

  if
    (not
       (((not my_node.is_open) && my_node.rate > 0)
       || ((not el_node.is_open) && el_node.rate > 0)))
    && rem_time < 3
  then ()
  else if rem_time < 2 then ()
  else if closed_v_sum = 0 then ()
  else if
    cur_pres + (rem_time * open_v_sum) + ((rem_time - 1) * closed_v_sum)
    <= !best_ref
  then (* No way we can beat the best result already *)
    ()
  else
    let my_can_open = (not my_node.is_open) && my_node.rate > 0 in
    let el_can_open = (not el_node.is_open) && el_node.rate > 0 in
    let do_move my el =
      if
        my <> my_prev_visited && my <> el_prev_visited && el <> my_prev_visited
        && el <> el_prev_visited
      then (
        let rinca =
          if my = my_idx then (
            my_node.is_open <- true;
            my_node.rate)
          else 0
        in
        let rincb =
          if el = el_idx then (
            assert (not el_node.is_open);
            el_node.is_open <- true;
            el_node.rate)
          else 0
        in
        solve_b data ~rem_time:(rem_time - 1) ~best_ref
          ~cur_pres:(cur_pres + open_v_sum)
          ~closed_v_sum:(closed_v_sum - rinca - rincb)
          ~open_v_sum:(open_v_sum + rinca + rincb)
          ~my_idx:my ~my_prev_visited:my_idx ~el_idx:el ~el_prev_visited:el_idx;

        if my = my_idx then my_node.is_open <- false;
        if el = el_idx then el_node.is_open <- false)
    in
    if my_can_open then product_iter do_move [ my_idx ] el_node.conn;

    (* If both on the same node, only one needs to try opening. *)
    if el_can_open && el_idx != my_idx then
      product_iter do_move my_node.conn [ el_idx ];

    (* Both might open at once *)
    if my_can_open && el_can_open && my_idx != el_idx then do_move my_idx el_idx;

    if my_idx != el_idx then product_iter do_move my_node.conn el_node.conn
    else
      (* If both are on the same node, don't need to try symmetric moves. *)
      asym_product_iter do_move my_node.conn

let sort_conns data =
  let sort_one node =
    let nc =
      List.sort (fun a b -> compare data.(b).rate data.(a).rate) node.conn
    in
    node.conn <- nc
  in
  Array.iter sort_one data

let read_data ic =
  let node_idxs = ref [] in
  let get_idx name =
    match List.assoc_opt name !node_idxs with
    | Some n -> n
    | None ->
        let n = List.length !node_idxs in
        node_idxs := (name, n) :: !node_idxs;
        n
  in
  let rec reader () =
    match input_line ic with
    | exception End_of_file -> []
    | ln ->
        let found = Str.string_match parse_regexp ln 0 in
        assert found;
        let cur_idx = get_idx (Str.matched_group 1 ln) in
        let rate = int_of_string (Str.matched_group 2 ln) in
        let conns =
          List.map
            (fun cl ->
              if String.get cl (String.length cl - 1) = ',' then
                get_idx (String.sub cl 0 (String.length cl - 1))
              else get_idx cl)
            (String.split_on_char ' ' (Str.matched_group 3 ln))
        in
        { idx = cur_idx; rate; conn = conns; is_open = false } :: reader ()
  in
  let reslist = reader () in
  let result = Array.make (List.length reslist) (List.hd reslist) in
  List.iter (fun ({ idx; _ } as x) -> result.(idx) <- x) reslist;

  (get_idx "AA", result)

let parta (start_idx, data) =
  sort_conns data;
  let best_ref = ref 0 in
  let closed_sum = Array.fold_left ( + ) 0 (Array.map (fun x -> x.rate) data) in
  solve data ~rem_time:30 ~best_ref ~cur_pres:0 ~closed_v_sum:closed_sum
    ~open_v_sum:0 ~node_idx:start_idx ~prev_visited:~-1;
  !best_ref

let partb (start_idx, data) =
  sort_conns data;
  let best_ref = ref 0 in
  let closed_sum = Array.fold_left ( + ) 0 (Array.map (fun x -> x.rate) data) in
  solve_b data ~rem_time:26 ~best_ref ~cur_pres:0 ~closed_v_sum:closed_sum
    ~open_v_sum:0 ~my_idx:start_idx ~my_prev_visited:~-1 ~el_idx:start_idx
    ~el_prev_visited:~-1;
  !best_ref

(*
   Runner
*)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
