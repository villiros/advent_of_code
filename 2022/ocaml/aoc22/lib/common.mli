type day_part = PartA | PartB
type prob_result = Int of int | Str of string

val verbose : bool ref
val log : (unit -> unit) -> unit
val logv : 'a -> unit
val assert_eq : 'a -> 'a -> unit
