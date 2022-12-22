(*
 * Day 13
 *)

open Common

type tpacket = PInt of int | PList of tpacket list
type compare_state = Good | Bad | Continue

let rec compare (a, b) =
  (* (comp = Continue) => next *)
  let ( => ) comp next =
    match compare comp with Continue -> compare next | r -> r
  in

  match (a, b) with
  | PList _, PInt _ -> compare (a, PList [ b ])
  | PInt _, PList _ -> compare (PList [ a ], b)
  (* *)
  | PList (ai :: ar), PList (bi :: br) -> (ai, bi) => (PList ar, PList br)
  | PList [], PList [] -> Continue
  | PList [], _ -> Good
  | _, PList [] -> Bad
  (* *)
  | PInt ai, PInt bi when ai < bi -> Good
  | PInt ai, PInt bi when ai > bi -> Bad
  | PInt _, PInt _ -> Continue

let read_data ic : (tpacket * tpacket) list =
  (* let input_char ic =
       if !verbose then
         let c = input_char ic in
         print_char c;
         c
       else
       input_char ic
     in *)

  (* XXX: This is a wee bit convoluted.
     Expects that on entry into read_packet the opening '[' has already been read.
     It goes until consuming the closing ']'
  *)
  let rec read_packet () =
    let exception Empty_List in
    let acc = ref [] in
    let cur = ref 'x' in
    (* *)
    try
      while !cur != ']' do
        cur := input_char ic;
        let value =
          match !cur with
          | '[' ->
              let res = read_packet () in
              (* Consume the ',' or our ']' *)
              cur := input_char ic;
              res
          | '0' .. '9' as c1 -> (
              (* Input can contain two-digit numbers, so deal with that (or detect ',' or ']')*)
              cur := input_char ic;
              match !cur with
              | '0' .. '9' as c2 ->
                  (* two-digit; consume the following ','/']'*)
                  cur := input_char ic;
                  PInt (int_of_string (Char.escaped c1 ^ Char.escaped c2))
              | ',' | ']' -> PInt (int_of_string (Char.escaped c1))
              | _ -> assert false)
          | ']' ->
              (* For non-empty list the closing ']' goes into cur and stops the loop.
                 But we get here if it's an empty list. *)
              assert (!acc = []);
              raise Empty_List
          | _ -> assert false
        in
        acc := value :: !acc
      done;
      PList (List.rev !acc)
    with Empty_List -> PList (List.rev !acc)
  in
  (* *)
  let rec read_pairs () =
    (* All of this is to detect if we're starting another pair of packets, or end of file*)
    match input_char ic with
    | '[' -> (
        let p1 = read_packet () in
        (* Go to the next packet in a pair *)
        assert_eq (input_char ic) '\n';
        assert_eq (input_char ic) '[';
        let p2 = read_packet () in
        (* Now skip over the eol and the blank line if there.*)
        match input_char ic with
        | '\n' -> (
            match input_char ic with
            | '\n' -> (p1, p2) :: read_pairs ()
            | _ -> assert false)
        | _ -> assert false
        | exception End_of_file -> [ (p1, p2) ])
    | _ -> assert false
  in
  read_pairs ()

let parta data =
  (* logv _data; *)
  List.fold_left ( + ) 0
    (List.mapi
       (fun i pair ->
         match compare pair with
         | Good ->
             logv ("Pair", i, "is good");
             i + 1
         | _o ->
             logv ("Pair", i, "is", _o);
             0)
       data)

let partb data =
  let order a b =
    match compare (a, b) with Good -> -1 | Bad -> 1 | Continue -> 0
  in
  let div1 = PList [ PList [ PInt 2 ] ] in
  let div2 = PList [ PList [ PInt 6 ] ] in
  let packets =
    div1 :: div2 :: List.fold_left (fun acc (a, b) -> a :: b :: acc) [] data
  in
  let packets = List.sort order packets in
  List.fold_left ( * ) 1
    (List.mapi
       (fun i pkt ->
         if compare (pkt, div1) = Continue || compare (pkt, div2) = Continue
         then i + 1
         else 1)
       packets)

(*
 * Runner
 *)

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
