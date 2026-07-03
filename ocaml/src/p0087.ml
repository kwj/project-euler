(* Project Euler: Problem 87 *)

open Core

module BitArray : sig
  type t

  val init : int -> t
  val set : t -> int -> unit
  val [@warning "-unused-value-declaration"] clear : t -> int -> unit
  val popcount : t -> int
end = struct
  type t =
    { word_size : int
    ; n_elems : int
    ; arr : int array
    }

  let init n =
    let word_size = Sys.int_size_in_bits in
    let arr_size = (n + word_size) / word_size in
    { word_size; n_elems = n; arr = Array.create ~len:arr_size 0 }
  ;;

  let set ba n =
    assert (n >= 0 && n <= ba.n_elems);
    let word_idx = n / ba.word_size
    and bit_idx = n mod ba.word_size in
    ba.arr.(word_idx) <- ba.arr.(word_idx) lor Int.shift_left 1 bit_idx
  ;;

  let clear ba n =
    assert (n >= 0 && n <= ba.n_elems);
    let word_idx = n / ba.word_size
    and bit_idx = n mod ba.word_size in
    ba.arr.(word_idx) <- (ba.arr.(word_idx) land Int.(bit_not (shift_left 1 bit_idx)))
  ;;

  let popcount ba = Array.fold ba.arr ~init:0 ~f:(fun acc x -> acc + Int.popcount x)
end

let compute thr =
  let ba = BitArray.init thr in
  let p_lst = Euler.Math.Prime.primes 1 (Euler.Math.isqrt thr) in
  let x2_lst = List.map p_lst ~f:(fun n -> n * n) in
  let y3_lst =
    List.(map p_lst ~f:(fun n -> n * n * n) |> take_while ~f:(fun x -> x < thr))
  in
  let z4_lst =
    List.(map x2_lst ~f:(fun n -> n * n) |> take_while ~f:(fun x -> x < thr))
  in
  List.(
    iter z4_lst ~f:(fun z4 ->
      iter y3_lst ~f:(fun y3 ->
        iter x2_lst ~f:(fun x2 ->
          if z4 + y3 + x2 < thr then BitArray.set ba (z4 + y3 + x2)))));
  BitArray.popcount ba
;;

let solve () = compute 50_000_000 |> Int.to_string

(* Test *)

let%test_unit "50" = [%test_eq: int] (compute 50) 4
let%test_unit "50_000_000" = [%test_eq: int] (compute 50_000_000) 1097343
