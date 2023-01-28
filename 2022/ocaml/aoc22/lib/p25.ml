(*
   Day 25
*)

open Common

let from_snafu digits =
  let folder digit (mul, acc) =
    (mul * 5, acc + (digit * mul))
  in
  let _, res = List.fold_right folder digits (1, 0) in
  res

let to_snafu v =
  let rec doer v mul =
    if v = 0 then []
    else (
      let rem = v mod mul in
      let digit, carry =
        match rem / (mul / 5) with
        | 0 -> '0', 0
        | 1 -> '1', 0
        | 2 -> '2', 0
        | 3 -> '=', mul
        | 4 -> '-', mul
        | _ -> assert false
      in
      digit :: doer (v - rem + carry) (mul * 5)
    )
  in
  let res = List.rev (doer v 5) in
  String.of_seq (List.to_seq res)

let rec read_data ic =
  let line_folder c acc = 
    match c with
    | '2' -> 2 :: acc
    | '1' -> 1 :: acc
    | '0' -> 0 :: acc
    | '-' -> -1 :: acc
    | '=' -> -2 :: acc
    | _ -> assert false
in
  match input_line ic with
  | exception End_of_file -> []
  | ln ->
    (String.fold_right line_folder ln []) :: read_data ic

let parta data =
  let data = List.map from_snafu data in
  logv (List.map to_snafu data);
  let sum = List.fold_left (+) 0 data in
  logv sum;
  to_snafu sum

(*
   Runner
*)

let solve part ic =
  match part with
  | PartA -> Str (parta (read_data ic))
  | PartB -> assert false