let tempo_name : string = "tempo"
let tempo_version : string = "0.1.0"
let tempo_description : string = "terminal tempo tapper"
let default_capacity : int = 10
let default_bounded : bool = true

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

let print_splash () =
  Printf.printf "%s %s: %s\n" tempo_name tempo_version tempo_description;
  Printf.printf "type \"h\" for help\n";
  ()

let print_prompt (t : Tempo.Tap.t) =
  Printf.printf "%d/%d%s samples in buffer\n" (Tempo.Tap.count t)
    (Tempo.Tap.capacity t)
    (if Tempo.Tap.is_bounded t then "" else "+");
  Printf.printf "%s BPM \n" (Tempo.Tap.string_of_sample (Tempo.Tap.bpm t));
  ()

let print_help =
  List.iter (fun c ->
      Printf.printf " %s or %s. %s.\n" (command_longname c)
        (command_shortname c) (command_description c))

let readln () = String.trim (read_line ())

let interactive_resize (t : Tempo.Tap.t) =
  Printf.printf "\n";
  Printf.printf " new buffer size? ";
  let input =
    try readln () with
    | End_of_file -> ""
  in
  let cap =
    match (input, int_of_string_opt input) with
    | "", _ -> None
    | _, Some n -> Some n
    | _, None ->
        (* TODO: not sure if it overflowed?? *)
        Printf.printf " invalid integer\n";
        None
  in
  match cap with
  | None -> t
  | Some cap ->
      let clamped = max 1 cap in
      let clamped = min (Tempo.Tap.max_capacity + 1) clamped in
      let t = Tempo.Tap.resize t clamped in
      let reported = Tempo.Tap.capacity t in
      if reported <> cap then
        Printf.printf " size too %s, clamped to %d\n"
          (if reported < cap then "large" else "small")
          reported;
      t

let rec repl t =
  Printf.printf "\n";
  print_prompt t;
  print_string (if Tempo.Tap.is_recording t then " * " else " ; ");
  let cmd =
    try command_of_string (readln ()) with
    | End_of_file -> Some Quit
  in
  let t =
    match cmd with
    | None ->
        Printf.printf "\n";
        Printf.printf " unrecognized command. try \"h\" for help.\n";
        Some t
    | Some cmd -> (
        match cmd with
        | Help ->
            Printf.printf "\n";
            print_help commands;
            Some t
        | Tap -> Some (Tempo.Tap.tap t)
        | Clear -> Some (Tempo.Tap.clear t)
        | Size -> Some (interactive_resize t)
        | Bound -> Some (Tempo.Tap.toggle_bounded t)
        | Print ->
            Printf.printf "\n";
            Printf.printf " %s\n" (Tempo.Tap.string_of_tapper t);
            Some t
        | Quit ->
            Printf.printf "\n";
            Printf.printf " goodbye\n";
            Printf.printf "\n";
            None)
  in
  match t with
  | Some t -> repl t
  | None -> ()

let main () =
  let t = Tempo.Tap.create default_capacity default_bounded in
  print_splash ();
  repl t

let () = main ()
