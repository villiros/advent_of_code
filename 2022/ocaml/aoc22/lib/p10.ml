(*
 * Day 5
 *)

open Common

type tinstr = Noop | Addx of int

module Cpu : sig
  type t

  val init : unit -> t
  val cycle : t -> int
  val get_x : t -> int
  val exec_program : t -> (t -> unit) -> tinstr list -> t
end = struct
  (* Internal execution state *)
  type tstate = Idle | Exec of { cmd : tinstr; cycle_left : int }

  (* Complete CPU state *)
  type t = { x : int; cycle : int; state : tstate }

  let cycle { cycle = c; _ } = c
  let get_x { x; _ } = x
  let init () = { x = 1; cycle = 1; state = Idle }

  (* Is it ready for another command? *)
  let ready = function { state = Idle; _ } -> true | _ -> false

  (* Start executing a new command. Does not trigger clock. *)
  let start cpu cmd =
    assert (ready cpu);
    match cmd with
    | Noop -> { cpu with state = Exec { cmd; cycle_left = 1 } }
    | Addx _ -> { cpu with state = Exec { cmd; cycle_left = 2 } }

  (* Trigger clock, if there's something to do still *)
  let pump = function
    | { state = Idle; _ } as c -> c
    | { state = Exec { cycle_left = 1; cmd = Noop }; _ } as c ->
        { c with cycle = c.cycle + 1; state = Idle }
    | { state = Exec { cycle_left = 1; cmd = Addx v }; _ } as c ->
        { cycle = c.cycle + 1; x = c.x + v; state = Idle }
    | { state = Exec ({ cycle_left = cl; _ } as estate); _ } as c when cl > 1 ->
        {
          c with
          cycle = c.cycle + 1;
          state = Exec { estate with cycle_left = estate.cycle_left - 1 };
        }
    | _ -> assert false

  (* Execute a complete program, calling the cb before each cycle increment (and once at the end). *)
  let rec exec_program cpu cycle_start_cb prog =
    match prog with
    | [] -> cpu
    | inst :: rest ->
        let cpu2 = ref (start cpu inst) in
        while not (ready !cpu2) do
          cycle_start_cb !cpu2;
          cpu2 := pump !cpu2
        done;
        exec_program !cpu2 cycle_start_cb rest
end

let read_data ic =
  let rec reader () =
    match input_line ic with
    | exception End_of_file -> []
    | "noop" -> Noop :: reader ()
    | ln -> (
        match String.split_on_char ' ' ln with
        | [ "addx"; n ] -> Addx (int_of_string n) :: reader ()
        | _ -> assert false)
  in
  reader ()

let parta data =
  let open Cpu in
  let result = ref 0 in
  let cb cpu =
    log (fun () ->
        Format.printf "At cycle %i value is %i\n" (cycle cpu) (get_x cpu));
    if (cycle cpu + 20) mod 40 == 0 then
      result := !result + (cycle cpu * get_x cpu)
  in
  ignore (exec_program (init ()) cb data);
  !result

let partb data =
  let open Cpu in
  let result = ref "" in
  let cb cpu =
    let row_pos = (cycle cpu - 1) mod 40 in
    (match get_x cpu - row_pos with
    | -1 | 0 | 1 -> result := !result ^ "#"
    | _ -> result := !result ^ ".");

    if cycle cpu mod 40 == 0 then result := !result ^ "\n"
  in
  ignore (exec_program (init ()) cb data);
  log (fun () -> print_string !result);
  (* For testing, put a hash of the result in the answers file *)
  let hash = Digest.to_hex (Digest.string !result) in
  hash

let solve part ic =
  match part with
  | PartA -> Int (parta (read_data ic))
  | PartB -> Str (partb (read_data ic))
