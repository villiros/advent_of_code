let () = Dum.default_lim := 1000
let param_problems = ref []
let prob_param name = param_problems := !param_problems @ [ name ]

let () =
  Arg.parse
    [ ("-p", Arg.String prob_param, "Day parts to run.") ]
    (fun _ -> raise (Arg.Bad "not allowed"))
    "h"

(*
 * Loading answers and filtering by cmdline
 *)
type answer = {
  probname : string;
  fname : string;
  result : int;
  result_known : bool;
}

let read_answers =
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
          | pname :: fname :: res :: _ ->
              let answ, result_known =
                if res = "?" then (0, false) else (int_of_string res, true)
              in
              reader state
              @ [ { probname = pname; fname; result = answ; result_known } ]
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
            (fun { probname = pb; _ } -> pb = param_probname)
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
  if ans.result_known then Format.printf " expect result %i\n" ans.result
  else Format.printf "\n";
  ()

(* Returns true if success *)
let check_result (ans : answer) result =
  match ans with
  | { result_known = false; _ } ->
      Format.printf " ???: Got %i\n" result;
      true
  | { result_known = true; result = expected; _ } when expected != result ->
      Format.printf " FAIL: Got %i Expected %i Diff %i\n" result expected
        (result - expected);
      false
  | { result_known = true; _ } ->
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
