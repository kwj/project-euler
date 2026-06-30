val permutation : int -> 'a list -> 'a list list
val permutation_with_repetition : int -> 'a list -> 'a list list
val combination : int -> 'a list -> 'a list list
val combination_with_repetition : int -> 'a list -> 'a list list
val powerset : 'a list -> 'a list list
val findall : ('a -> bool) -> 'a list -> int list

(* misc. *)
val list_to_str : ('a -> string) -> string -> 'a list -> string
val list_assoc_group : ('a * 'b) list -> ('a * 'b list) list

(* from Julia *)
val digits : ?base:int -> int -> int list
val undigits : ?base:int -> int list -> int
val z_digits : ?base:int -> Z.t -> int list
val z_undigits : ?base:int -> int list -> Z.t
