type day_part = PartA | PartB
type prob_result = Int of int | Str of string

let verbose = ref false
let log fn = if !verbose then fn () else ()

let logv v =
  if !verbose then (
    Dum.to_stdout v;
    print_newline ())
  else ()

let assert_eq a b =
  if a = b then ()
  else
    Format.printf "assert_eq FAIL: %s != %s\n" (Dum.to_string a)
      (Dum.to_string b);
  assert (a = b)
