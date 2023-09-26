(* Project Euler: Problem 50 *)

open Core

let cumsum_generator () =
  let prime = ref 0 in
  let acc = ref 0 in
  let next () =
    prime := Euler.Math.Prime.next_prime !prime;
    acc := !acc + !prime;
    !acc
  in
  next
;;

let init_cumsum_lst cs_gen limit =
  let rec aux lst = if List.hd_exn lst < limit then aux (cs_gen () :: lst) else lst in
  aux [ 0 ]
;;

let compute limit =
  let cs_gen = cumsum_generator () in
  let cs_lst = init_cumsum_lst cs_gen limit in
  let begin_pos = List.length cs_lst - 1 in
  let rec aux cs_lst width ans =
    if List.nth_exn cs_lst (begin_pos - width) - List.nth_exn cs_lst begin_pos >= limit
    then ans
    else (
      let lst = List.slice cs_lst 0 (begin_pos - width + 1) in
      match
        List.drop_while lst ~f:(fun n ->
          let x = n - List.nth_exn cs_lst begin_pos in
          x >= limit || Bool.(Euler.Math.Prime.is_prime x = false))
      with
      | [] -> aux (cs_gen () :: cs_lst) width ans
      | l ->
        aux
          (cs_gen () :: cs_lst)
          (width + List.length l)
          (List.nth_exn l 0 - List.nth_exn cs_lst begin_pos))
  in
  aux cs_lst 1 0
;;

let solve () = compute 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "100" = [%test_eq: int] (compute 100) 41
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 953
let%test_unit "1_000_000" = [%test_eq: int] (compute 1_000_000) 997651
