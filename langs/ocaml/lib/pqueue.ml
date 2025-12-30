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
