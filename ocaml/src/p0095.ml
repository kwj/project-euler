(* Project Euler: Problem 95 *)

open Core

let make_spd_tbl limit =
  let d_tbl = Euler.Math.get_sigma_tbl 1 limit in
  Array.iteri ~f:(fun idx _ -> d_tbl.(idx) <- d_tbl.(idx) - idx) d_tbl;
  d_tbl
;;

let update_chain_tbl tbl lst v =
  let rec loop = function
    | [] -> ()
    | x :: xs ->
      tbl.(x) <- v;
      loop xs
  in
  loop lst
;;

let compute limit =
  let spd_tbl = make_spd_tbl limit in
  let chain_tbl = Array.create ~len:(limit + 1) 0 in
  let chain = Queue.create () in
  let max_length = ref 0 in

  Sequence.range 2 limit ~stop:`inclusive
  |> Sequence.iter ~f:(fun i ->
    Queue.clear chain;
    let pos = ref i in
    let rec loop_chain () =
      if chain_tbl.(!pos) = 0
      then (
        Queue.enqueue chain !pos;
        pos := spd_tbl.(!pos);
        if
          !pos > 1
          && !pos <= limit
          && Bool.(Queue.exists chain ~f:(fun n -> Int.equal n !pos) = false)
        then loop_chain ())
    in
    loop_chain ();

    if !pos <= 1 || !pos > limit || chain_tbl.(!pos) <> 0
    then update_chain_tbl chain_tbl (Queue.to_list chain) (-1)
    else (
      let rec aux i = if !pos <> Queue.get chain i then aux (succ i) else i in
      let i = aux 0 in
      let length = Queue.length chain - i + 1 in
      update_chain_tbl chain_tbl (List.take (Queue.to_list chain) i) (-1);
      update_chain_tbl chain_tbl (List.drop (Queue.to_list chain) i) length;
      max_length := Int.max !max_length length));
  let ans, _ =
    Array.findi chain_tbl ~f:(fun _ n -> n = !max_length) |> Option.value_exn
  in
  ans
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 14316
