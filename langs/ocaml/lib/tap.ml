let max_capacity : int = 0x1000

type t = {
  samples : float Pqueue.t;
  prev_tap : float option;
  capacity : int;
  bounded : bool;
}

let create (capacity : int) (bounded : bool) : t =
  { samples = Pqueue.create max_capacity; prev_tap = None; capacity; bounded }

let count t = Pqueue.length t.samples
let capacity t = min t.capacity (Pqueue.capacity t.samples)
let is_recording t = not (t.prev_tap = None)
let is_bounded t = t.bounded

let sync_cap t =
  if is_bounded t then
    { t with samples = Pqueue.truncate_back t.samples (capacity t) }
  else t

let toggle_bounded t = sync_cap { t with bounded = not t.bounded }
let clear t = { t with samples = Pqueue.clear t.samples; prev_tap = None }
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
          let t = { t with samples = Pqueue.push t.samples bpm } in
          sync_cap t
        else t
  in
  { t with prev_tap = Some now }

let bpm t =
  let avg, _ =
    Pqueue.fold
      (fun acc s ->
        let avg, idx = acc in
        let avg = avg +. ((s -. avg) /. float_of_int idx) in
        (avg, idx + 1))
      (0.0, 1) t.samples
  in
  avg

let string_of_sample s = Printf.sprintf "%.1f" s

let string_of_tapper t =
  Pqueue.string_of_queue string_of_sample (Pqueue.rev t.samples)
