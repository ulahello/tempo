type t

val max_capacity : int
val create : int -> bool -> t
val tap : t -> t
val push_bpm : t -> float -> t
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
val to_list : t -> float list
