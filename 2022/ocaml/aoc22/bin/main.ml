let () = Dum.default_lim := 1000
let param_problems = ref []
let prob_param name = param_problems := !param_problems @ [ name ]

let () =
  Arg.parse
    [
      ("-p", Arg.String prob_param, "Day parts to run.");
      ("-v", Arg.Set Aoc22.Common.verbose, "Output verbose logs");
    ]
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
                | '"' -> (Str (String.sub res 1 (String.length res - 2)), true)
                | '0' .. '9' -> (Int (int_of_string res), true)
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
let check_result (ans : answer) result time =
  match ans with
  | { result_known = false; _ } ->
      Format.printf " ???: Got %s Time: %.3fms\n" (result_to_string result)
        (time *. 1000.);
      true
  | { result_known = true; result = expected; _ } when expected <> result ->
      Format.printf " FAIL: Got %s Expected %s Diff %s Time: %.3fms\n"
        (result_to_string result)
        (result_to_string expected)
        (match (expected, result) with
        | Int e, Int r -> string_of_int (e - r)
        | Str _, Str _ -> "???"
        | _ -> assert false)
        (time *. 1000.);
      false
  | { result_known = true; _ } ->
      assert (ans.result = result);
      Format.printf " OK Time: %.3fms\n" (time *. 1000.);
      true

let exec_solve ic probname =
  let open Aoc22 in
  let start_time = Unix.gettimeofday () in
  let result =
    try
      match probname with
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
      | "p06a" -> P06.solve PartA ic
      | "p06b" -> P06.solve PartB ic
      | "p07a" -> P07.solve PartA ic
      | "p07b" -> P07.solve PartB ic
      | "p08a" -> P08.solve PartA ic
      | "p08b" -> P08.solve PartB ic
      | "p09a" -> P09.solve PartA ic
      | "p09b" -> P09.solve PartB ic
      | "p10a" -> P10.solve PartA ic
      | "p10b" -> P10.solve PartB ic
      | "p11a" -> P11.solve PartA ic
      | "p11b" -> P11.solve PartB ic
      | "p12a" -> P12.solve PartA ic
      | "p12b" -> P12.solve PartB ic
      | "p13a" -> P13.solve PartA ic
      | "p13b" -> P13.solve PartB ic
      | "p14a" -> P14.solve PartA ic
      | "p14b" -> P14.solve PartB ic
      | "p15a" -> P15.solve PartA ic
      | "p15b" -> P15.solve PartB ic
      | "p16a" -> P16.solve PartA ic
      | "p16b" -> P16.solve PartB ic
      | "p17a" -> P17.solve PartA ic
      | "p17b" -> P17.solve PartB ic
      | "p18a" -> P18.solve PartA ic
      | "p18b" -> P18.solve PartB ic
      | "p19a" -> P19.solve PartA ic
      | "p19b" -> P19.solve PartB ic
      | "p20a" -> P20.solve PartA ic
      | "p20b" -> P20.solve PartB ic
      | "p21a" -> P21.solve PartA ic
      | "p21b" -> P21.solve PartB ic
      | "p22a" -> P22.solve PartA ic
      | "p22b" -> P22.solve PartB ic
      | "p23a" -> P23.solve PartA ic
      | "p23b" -> P23.solve PartB ic
      | _ -> assert false
    with exc ->
      close_in_noerr ic;
      raise exc
  in
  (result, Unix.gettimeofday () -. start_time)

let run_case (ans : answer) =
  print_run_header ans;
  flush stdout;
  let result, time =
    let ic = open_in ("../input/" ^ ans.fname) in
    exec_solve ic ans.probname
  in
  let res = check_result ans result time in
  flush stdout;
  res

let () =
  let outcomes = List.map run_case cases_to_run in
  if List.for_all (fun x -> x) outcomes then (
    print_endline "ALL OK";
    exit 0)
  else (
    print_endline "FAILURES";
    exit 1)
