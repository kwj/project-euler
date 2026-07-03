(* Project Euler: Problem 60 *)

open Core
module M = Euler.Math

let get_pairable_primes x desc_ps curr_minsum =
  let upper_x = M.int_pow 10 (M.num_of_digits x) in
  let check_pair p upper_p =
    M.Prime.(is_prime ((x * upper_p) + p) && is_prime ((p * upper_x) + x))
  in
  let rec get_under_p p under_p =
    if p > under_p then under_p else get_under_p p (under_p / 10)
  in
  let rec aux result under_p = function
    | [] -> List.rev result
    | p :: ps ->
      if x + p >= curr_minsum
      then aux result under_p ps
      else (
        let new_under_p = get_under_p p under_p in
        match check_pair p (new_under_p * 10) with
        | true -> aux (p :: result) new_under_p ps
        | _ -> aux result new_under_p ps)
  in
  aux [] (M.int_pow 10 (M.get_max_exp (List.hd_exn desc_ps))) desc_ps
;;

let find_cliques p desc_nbr_lst size tbl =
  let result = ref [] in
  let rec aux group nbrs depth =
    if depth = 0
    then result := (p :: group) :: !result
    else
      for offset = 0 to List.length nbrs - depth do
        let elem = List.nth_exn nbrs offset in
        if List.for_all group ~f:(fun x -> Set.mem (Hashtbl.find_exn tbl x) elem)
        then (
          let _, rest_nbrs = List.split_n nbrs offset in
          aux (elem :: group) rest_nbrs (pred depth))
      done
  in
  aux [] desc_nbr_lst size;
  !result
;;

let compute size_of_clique =
  let size = size_of_clique - 1 in
  let prime_lst = [| ref [ 3 ]; ref [ 3 ] |] in
  let tbl = Hashtbl.create (module Int) in
  Hashtbl.set tbl ~key:3 ~data:Int.Set.empty;

  let rec loop prev_p answer =
    let p = M.Prime.next_prime prev_p in

    if p >= answer
    then answer
    else (
      let idx = (p + 2) mod 3 in
      let desc_nbr_lst = get_pairable_primes p !(prime_lst.(idx)) answer in
      Hashtbl.set tbl ~key:p ~data:(Int.Set.of_list desc_nbr_lst);
      prime_lst.(idx) := p :: !(prime_lst.(idx));

      if List.length desc_nbr_lst < size_of_clique - 1
      then loop p answer
      else (
        match find_cliques p desc_nbr_lst size tbl with
        | [] -> loop p answer
        | cliques ->
          let min_sum_clique =
            List.(
              map cliques ~f:(reduce_exn ~f:( + ))
              |> min_elt ~compare:Int.compare
              |> Option.value_exn)
          in
          loop p (min answer min_sum_clique)))
  in
  loop 5 Int.max_value
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 792
let%test_unit "5" = [%test_eq: int] (compute 5) 26033
