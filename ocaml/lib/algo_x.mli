type t

val dlx_init : int -> t
val dlx_add_row : ?name:string -> t -> int list -> unit
val dlx_solve : t -> string list list option
