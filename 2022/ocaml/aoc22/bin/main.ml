let () = Dum.default_lim := 1000
let param_problems = ref []
let prob_param name = param_problems := !param_problems @ [ name ]

let () =
  Arg.parse
    [ ("-p", Arg.String prob_param, "Day parts to run.") ]
    (fun _ -> raise (Arg.Bad "not allowed"))
    "h"

open Aoc22.Common

(*
 * Loading answers and filtering by cmdline
 *)
type answer = {
  probname : string;
  fname : string;
  result : Aoc22.Common.prob_result;
  result_known : bool;
}

let result_to_string = function
  | Aoc22.Common.Int r -> string_of_int r
  | Aoc22.Common.Str s -> s

let read_answers : answer list =
  let open Aoc22.Common in
  let exception BadInputLine of string in
  let ic = open_in "../answers" in
  let rec reader state =
    match input_line ic with
    | exception End_of_file -> state
    | "" -> reader state
    | ln -> (
        if Str.first_chars ln 1 = "#" then reader state
        else
          match Str.split (Str.regexp "[ \t]+") ln with
          | probname :: fname :: (res : string) :: _ when String.length res > 0
            ->
              let answ, result_known =
                match res.[0] with
                | '?' -> (Int 0, false)
                | '0' .. '9' -> (Int (int_of_string res), true)
                | 'a' .. 'z' | 'A' .. 'Z' -> (Str res, true)
                | _ -> raise (BadInputLine ("Bad input line: " ^ ln))
              in
              reader state
              @ [ { probname; fname; result = answ; result_known } ]
          | _ -> raise (BadInputLine ("Bad input line: " ^ ln)))
  in
  List.rev (reader [])

let cases_to_run =
  let all_answers = read_answers in
  if !param_problems = [] then all_answers
  else
    List.fold_left
      (fun acc param_probname ->
        match
          List.filter
            (fun { probname; _ } -> probname = param_probname)
            all_answers
        with
        | [] -> assert false
        | matched_ans -> acc @ matched_ans)
      [] !param_problems

(*
 * Running a problem solve
 *)
let print_run_header (ans : answer) =
  Format.printf "# Running %s on %s" ans.probname ans.fname;
  if ans.result_known then
    Format.printf " expect result %s\n" (result_to_string ans.result)
  else Format.printf "\n";
  ()

(* Returns true if success *)
let check_result (ans : answer) result =
  match ans with
  | { result_known = false; _ } ->
      Format.printf " ???: Got %s\n" (result_to_string result);
      true
  | { result_known = true; result = expected; _ } when expected <> result ->
      Format.printf " FAIL: Got %s Expected %s Diff %s\n"
        (result_to_string result)
        (result_to_string expected)
        (match (expected, result) with
        | Int e, Int r -> string_of_int (e - r)
        | Str _, Str _ -> "???"
        | _ -> assert false);
      false
  | { result_known = true; _ } ->
      assert (ans.result = result);
      Format.printf " OK\n";
      true

let run_case (ans : answer) =
  let open Aoc22 in
  print_run_header ans;
  check_result ans
    (let ic = open_in ("../input/" ^ ans.fname) in
     try
       match ans.probname with
       | "p01a" -> P01.solve PartA ic
       | "p01b" -> P01.solve PartB ic
       | "p02a" -> P02.solve PartA ic
       | "p02b" -> P02.solve PartB ic
       | "p03a" -> P03.solve PartA ic
       | "p03b" -> P03.solve PartB ic
       | "p04a" -> P04.solve PartA ic
       | "p04b" -> P04.solve PartB ic
       | "p05a" -> P05.solve PartA ic
       | "p05b" -> P05.solve PartB ic
       | _ -> assert false
     with exc ->
       close_in_noerr ic;
       raise exc)

let () =
  let outcomes = List.map run_case cases_to_run in
  if List.for_all (fun x -> x) outcomes then (
    print_endline "ALL OK";
    exit 0)
  else (
    print_endline "FAILURES";
    exit 1)
