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
