let tempo_name : string = "tempo"
let tempo_version : string = "0.1.0"
let tempo_description : string = "terminal tempo tapper"
let default_capacity : int = 10
let default_bounded : bool = true

module type PQUEUE = sig
  type 'a t

  val create : int -> 'a t
  val length : 'a t -> int
  val capacity : 'a t -> int
  val is_empty : 'a t -> bool
  val is_full : 'a t -> bool
  val push : 'a t -> 'a -> 'a t
  val push_strict : 'a t -> 'a -> 'a t option
  val pop : 'a t -> 'a t * 'a option
  val clear : 'a t -> 'a t
  val rev : 'a t -> 'a t
  val truncate_back : 'a t -> int -> 'a t
  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val string_of_queue : ('a -> string) -> 'a t -> string
end

module type TAP = sig
  type t

  val max_capacity : int
  val create : int -> bool -> t
  val tap : t -> t
  val clear : t -> t
  val resize : t -> int -> t
  val toggle_bounded : t -> t
  val bpm : t -> float
  val count : t -> int
  val capacity : t -> int
  val is_recording : t -> bool
  val is_bounded : t -> bool
  val string_of_sample : float -> string
  val string_of_tapper : t -> string
end

module PQueue : PQUEUE = struct
  type 'a t = {
    front : 'a list;
    back : 'a list;
    capacity : int;
  }

  let create (capacity : int) : 'a t = { front = []; back = []; capacity }
  let length t = List.length t.front + List.length t.back
  let capacity t = t.capacity
  let is_empty t = length t = 0
  let is_full t = length t = capacity t

  let rebalance = function
    | { front = []; back; capacity } ->
        { front = List.rev back; back = []; capacity }
    | t -> t

  let push_strict t x =
    if is_full t then None
    else
      let t = { t with back = x :: t.back } in
      Some t

  let pop t =
    if is_empty t then (t, None)
    else
      let t = rebalance t in
      let x, xs =
        match t.front with
        | [] -> failwith "unreachable"
        | x :: xs -> (x, xs)
      in
      ({ t with front = xs }, Some x)

  let push t x =
    let t = if is_full t then pop t |> fst else t in
    push_strict t x |> Option.get

  let clear t = { t with front = []; back = [] }
  let rev t = { t with front = t.back; back = t.front }

  let rec truncate_back t new_len =
    if length t <= new_len then t
    else
      let t, _ = pop t in
      truncate_back t new_len

  let fold f init t =
    let front_acc = List.fold_left f init t.front in
    List.fold_left f front_acc (List.rev t.back)

  let string_of_queue fmt t =
    let xs : 'a list = t.front @ List.rev t.back in
    "[" ^ String.concat ", " (List.map fmt xs) ^ "]"
end

module Tap : TAP = struct
  let max_capacity : int = 0x1000

  type t = {
    samples : float PQueue.t;
    prev_tap : float option;
    capacity : int;
    bounded : bool;
  }

  let create (capacity : int) (bounded : bool) : t =
    { samples = PQueue.create max_capacity; prev_tap = None; capacity; bounded }

  let count t = PQueue.length t.samples
  let capacity t = min t.capacity (PQueue.capacity t.samples)
  let is_recording t = not (t.prev_tap = None)
  let is_bounded t = t.bounded
  let toggle_bounded t = { t with bounded = not t.bounded }

  let sync_cap t =
    if is_bounded t then
      { t with samples = PQueue.truncate_back t.samples (capacity t) }
    else t

  let clear t = { t with samples = PQueue.clear t.samples; prev_tap = None }
  let resize t new_cap = sync_cap { t with capacity = new_cap }

  let tap t =
    let now = Unix.gettimeofday () in
    let t =
      match t.prev_tap with
      | None -> t
      | Some prev ->
          if prev < now then
            let elapsed_secs = now -. prev in
            let bpm = 60.0 /. elapsed_secs in
            let t = { t with samples = PQueue.push t.samples bpm } in
            sync_cap t
          else t
    in
    { t with prev_tap = Some now }

  let bpm t =
    let avg, _ =
      PQueue.fold
        (fun acc s ->
          let avg, idx = acc in
          let avg = avg +. ((s -. avg) /. float_of_int idx) in
          (avg, idx + 1))
        (0.0, 1) t.samples
    in
    avg

  let string_of_sample s = Printf.sprintf "%.1f" s

  let string_of_tapper t =
    PQueue.string_of_queue string_of_sample (PQueue.rev t.samples)
end

type command =
  | Help
  | Tap
  | Clear
  | Size
  | Bound
  | Print
  | Quit

let commands : command list = [ Help; Tap; Clear; Size; Bound; Print; Quit ]

let command_literal : command -> string = function
  | Help -> "h"
  | Tap -> ""
  | Clear -> "c"
  | Size -> "s"
  | Bound -> "b"
  | Print -> "p"
  | Quit -> "q"

let command_shortname : command -> string = function
  | Tap -> "<enter>"
  | c -> command_literal c

let command_longname : command -> string = function
  | Help -> "help"
  | Tap -> "tap"
  | Clear -> "clear"
  | Size -> "size"
  | Bound -> "bound"
  | Print -> "print"
  | Quit -> "quit"

let command_description : command -> string = function
  | Help -> "describe commands"
  | Tap -> "register a beat"
  | Clear -> "clear buffer contents"
  | Size -> "adjust buffer size"
  | Bound -> "toggle whether buffer is bounded to size"
  | Print -> "print buffer contents in order from newest to oldest"
  | Quit -> "quit"

let command_of_string s =
  List.find_map
    (fun cmd ->
      List.find_map
        (fun repr ->
          if String.lowercase_ascii s = String.lowercase_ascii repr then
            Some cmd
          else None)
        [ command_literal cmd; command_longname cmd ])
    commands

let print_splash (_ : unit) : unit =
  let () =
    Printf.printf "%s %s: %s\n" tempo_name tempo_version tempo_description
  in
  let () = Printf.printf "type \"h\" for help\n" in
  ()

let print_prompt (t : Tap.t) : unit =
  let () =
    Printf.printf "%d/%d%s samples in buffer\n" (Tap.count t) (Tap.capacity t)
      (if Tap.is_bounded t then "" else "+")
  in
  let () = Printf.printf "%s BPM \n" (Tap.string_of_sample (Tap.bpm t)) in
  ()

let print_help (_ : unit) : unit =
  let rec h = function
    | [] -> ()
    | c :: cs ->
        let () =
          Printf.printf " %s or %s. %s.\n" (command_longname c)
            (command_shortname c) (command_description c)
        in
        h cs
  in
  h commands

let readln (_ : unit) = String.trim (read_line ())

let interactive_resize (t : Tap.t) =
  let () = Printf.printf "\n" in
  let () = Printf.printf " new buffer size? " in
  let input =
    try readln () with
    | End_of_file -> ""
  in
  if input = "" then t
  else
    let cap =
      match int_of_string_opt input with
      | Some n -> Some n
      | None ->
          (* TODO: not sure if it overflowed?? *)
          let () = Printf.printf " invalid integer\n" in
          None
    in
    match cap with
    | None -> t
    | Some cap ->
        let clamped = max 1 cap in
        let clamped = min (Tap.max_capacity + 1) clamped in
        let t = Tap.resize t clamped in
        let reported = Tap.capacity t in
        if reported = cap then t
        else
          let () =
            Printf.printf " size too %s, clamped to %d\n"
              (if reported < cap then "large" else "small")
              reported
          in
          t

let rec repl t =
  let () = Printf.printf "\n" in
  let () = print_prompt t in

  let () = print_string (if Tap.is_recording t then " * " else " ; ") in
  let cmd =
    try command_of_string (readln ()) with
    | End_of_file -> Some Quit
  in
  let t =
    match cmd with
    | None ->
        let () = Printf.printf "\n" in
        let () = Printf.printf " unrecognized command. try \"h\" for help.\n" in
        Some t
    | Some cmd -> (
        match cmd with
        | Help ->
            let () = Printf.printf "\n" in
            let () = print_help () in
            Some t
        | Tap -> Some (Tap.tap t)
        | Clear -> Some (Tap.clear t)
        | Size -> Some (interactive_resize t)
        | Bound -> Some (Tap.toggle_bounded t)
        | Print ->
            let () = Printf.printf "\n" in
            let () = Printf.printf " %s\n" (Tap.string_of_tapper t) in
            Some t
        | Quit ->
            let () = Printf.printf "\n" in
            let () = Printf.printf " goodbye\n" in
            let () = Printf.printf "\n" in
            None)
  in
  match t with
  | Some t -> repl t
  | None -> ()

let main (_ : unit) =
  let t = Tap.create default_capacity default_bounded in
  let () = print_splash () in
  repl t

let () = main ()
