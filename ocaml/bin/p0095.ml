(* Project Euler: Problem 95 *)

(*
  This program is slow.

  % /bin/time -p _build/default/bin/p0095.exe
  14316 [length of chain=28]
  Elapsed time: 5.1873927116394043s
  real 5.21
  user 5.10
  sys 0.09
 *)

open Core

let make_divsum_tbl limit =
  let module P = Euler.Math.Prime in
  let prime_t = P.make_tables limit in
  let tbl = Array.init (limit + 1)
              ~f:(fun i -> if i < 2 then 0 else
                             List.fold ~init:0 ~f:(+) (P.divisors prime_t i) - i) in
  tbl

let solve limit =
  let module PQ = Euler.PrioQueue.Make(struct
                      type t = int * int list
                      let compare x y = Int.compare (fst x) (fst y)
                    end) in
  let divsum_tbl = make_divsum_tbl limit in
  let chain_tbl = Array.create ~len:(limit + 1) 0 in
  let chain start_idx =
    let rec aux n acc =
      if n > limit || n < 2 then
        None
      else
        if chain_tbl.(n) = start_idx then
          Some (List.drop_while (List.rev acc) ~f:((<>) n))
        else
          if chain_tbl.(n) <> 0 then
            None
          else (
            chain_tbl.(n) <- start_idx;
            aux divsum_tbl.(n) (n :: acc)
          )
    in
    aux start_idx []
  in

  let pq = PQ.init () in
  let rec loop num =
    if num < 1 then
      PQ.peek pq
    else
      match chain num with
        None -> loop (pred num)
      | Some lst -> PQ.insert pq (List.length lst, lst); loop (pred num)
  in
  loop limit

let exec () =
  let len, lst = solve (1_000_000) in
  sprintf "%d [length of chain=%d]" (List.hd_exn (List.sort lst ~compare)) len

let () = Euler.Task.run exec
