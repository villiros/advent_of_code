(*
 * Day 11
 *)

open Common

let rec last = function a :: [] -> a | _ :: x -> last x | [] -> assert false

let rec seq from to_inclusive =
  if from <= to_inclusive then from :: seq (from + 1) to_inclusive else []

type toperation = Add of int | Mult of int | Square
type ttest = Div of int

class monkey pitems pop ptest ptrue_throw pfalse_throw =
  object
    val items = Queue.create ()
    val op = pop
    val test = match ptest with Div x -> x
    val true_throw = ptrue_throw
    val false_throw = pfalse_throw
    val mutable num_inspects = 0
    method throw_in item = Queue.add item items
    method get_num_inspects = num_inspects

    (* Part 2 modulus; all tests are primes in this *)
    val modulus = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23

    (* Part 1: do_turn_div = true; Part 2: false, does modulo arithmetic instead*)
    method run_turn (env : monkey array) do_turn_div =
      let rec runner () =
        if Queue.is_empty items then ()
        else
          let item = Queue.pop items in
          let item =
            match op with
            | Add x -> item + x
            | Mult x -> item * x
            | Square -> item * item
          in
          let item = if do_turn_div then item / 3 else item mod modulus in
          let throw_to =
            if item mod test == 0 then true_throw else false_throw
          in
          num_inspects <- num_inspects + 1;
          env.(throw_to)#throw_in item;
          runner ()
      in
      runner ()

    initializer
    List.iter (fun n -> Queue.add n items) pitems;
    assert (modulus mod test = 0)
  end

let parse_regexp =
  Str.regexp
    {|[ ]+Starting items: \(.+\)
[ ]+Operation: new = old \(.\) \(.+\)
[ ]+Test: divisible by \(.+\)
[ ]+If true: throw to monkey \(.+\)
[ ]+If false: throw to monkey \(.+\)|}
(*
   {|  Starting items: 79, 98
   Operation: new = old * 19
   Test: divisible by 23
     If true: throw to monkey 2
     If false: throw to monkey 3|} *)

let read_one ic expect_num =
  let open String in
  let has_data =
    match split_on_char ' ' (input_line ic) with
    | [ "Monkey"; n ] when n = string_of_int expect_num ^ ":" -> true
    | exception End_of_file -> false
    | _ -> assert false
  in
  if has_data then (
    let istr = concat "\n" (List.map (fun _ -> input_line ic) (seq 1 5)) in
    let found = Str.string_match parse_regexp istr 0 in
    log (fun () -> Format.printf "Input:\n%s\n" istr);
    assert found;
    let items =
      List.map
        (fun x ->
          if contains x ',' then int_of_string (sub x 0 (length x - 1))
          else int_of_string x)
        (split_on_char ' ' (Str.matched_group 1 istr))
    in
    let op =
      match (Str.matched_group 2 istr, Str.matched_group 3 istr) with
      | "*", "old" -> Square
      | "*", n -> Mult (int_of_string n)
      | "+", n -> Add (int_of_string n)
      | _ -> assert false
    in
    let test = Div (int_of_string (Str.matched_group 4 istr)) in
    let true_throw = int_of_string (Str.matched_group 5 istr) in
    let false_throw = int_of_string (Str.matched_group 6 istr) in

    (* Optional newline *)
    (match input_line ic with "" -> () | _ | (exception End_of_file) -> ());

    Some (new monkey items op test true_throw false_throw))
  else None

let read_data ic =
  let rec reader i =
    match read_one ic i with None -> [] | Some m -> m :: reader (i + 1)
  in
  let monkey_list = reader 0 in
  Array.of_list monkey_list

let parta data =
  for _ = 1 to 20 do
    Array.iter (fun x -> x#run_turn data true) data
  done;
  match
    List.sort
      (fun a b -> b - a)
      (Array.fold_left (fun acc i -> i#get_num_inspects :: acc) [] data)
  with
  | a :: b :: _ -> a * b
  | _ -> assert false

let partb data =
  for _ = 1 to 10000 do
    Array.iter (fun x -> x#run_turn data false) data
  done;
  match
    List.sort
      (fun a b -> b - a)
      (Array.fold_left (fun acc i -> i#get_num_inspects :: acc) [] data)
  with
  | a :: b :: _ -> a * b
  | _ -> assert false

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Int (partb (read_data ic))
