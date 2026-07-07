(* Project Euler: Problem 95 *)

open Core

let compute limit =
  let spd_tbl = Euler.Math.aliquot_sum_tbl limit in
  let chain_tbl = Array.create ~len:(limit + 1) 0 in
  let update_chain_tbl tbl lst v = List.iter lst ~f:(fun i -> tbl.(i) <- v) in
  let chain = Queue.create () in
  let max_len = ref 0 in

  Sequence.range 2 limit ~stop:`inclusive
  |> Sequence.iter ~f:(fun i ->
    Queue.clear chain;
    let pos = ref i in
    let rec loop_chain () =
      if chain_tbl.(!pos) = 0
      then (
        Queue.enqueue chain !pos;
        pos := spd_tbl.(!pos);
        if !pos > 1 && !pos <= limit && not (Queue.exists chain ~f:(( = ) !pos))
        then loop_chain ())
    in
    loop_chain ();

    if !pos <= 1 || !pos > limit || chain_tbl.(!pos) <> 0
    then update_chain_tbl chain_tbl (Queue.to_list chain) (-1)
    else (
      let rec aux i = if !pos <> Queue.get chain i then aux (succ i) else i in
      let idx = aux 0 in
      let len = Queue.length chain - idx + 1 in
      let lst_a, lst_b = List.split_n (Queue.to_list chain) idx in
      update_chain_tbl chain_tbl lst_a (-1);
      update_chain_tbl chain_tbl lst_b len;
      max_len := Int.max !max_len len));

  Array.findi_exn chain_tbl ~f:(fun _ n -> n = !max_len) |> fst
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 14316
