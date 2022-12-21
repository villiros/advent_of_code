type day_part = PartA | PartB
type prob_result = Int of int | Str of string

let verbose = ref false
let log fn = if !verbose then fn () else ()
