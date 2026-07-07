(* Project Euler: Problem 60 *)

open Core
module M = Euler.Math

let get_pairable_primes x desc_ps curr_minsum =
  let upper_x = M.int_pow 10 (M.num_of_digits x) in

  let is_concat_prime p upper_p =
    M.Prime.(is_prime ((x * upper_p) + p) && is_prime ((p * upper_x) + x))
  in
  let rec get_under_p p under_p =
    if p > under_p then under_p else get_under_p p (under_p / 10)
  in
  let rec aux result under_p = function
    | [] -> List.rev result (* Return pairable primes in descending order *)
    | p :: ps ->
      if x + p >= curr_minsum
      then aux result under_p ps
      else (
        let new_under_p = get_under_p p under_p in
        if is_concat_prime p (new_under_p * 10)
        then aux (p :: result) new_under_p ps
        else aux result new_under_p ps)
  in
  aux [] (M.int_pow 10 (M.get_max_exp (List.hd_exn desc_ps))) desc_ps
;;

let find_cliques desc_nbr_primes clique_size tbl =
  let result = ref [] in

  let rec aux clq nbrs =
    if List.length clq = clique_size
    then result := clq :: !result
    else
      List.(
        take nbrs (length nbrs - (clique_size - length clq) + 1)
        |> iteri ~f:(fun idx elem ->
          if for_all clq ~f:Fun.(compose (flip Set.mem elem) (Hashtbl.find_exn tbl))
          then aux (elem :: clq) (split_n nbrs idx |> snd)))
  in
  aux [] desc_nbr_primes;
  !result
;;

let compute target_size =
  let search_clique_size = target_size - 1 in
  let prime_lst = [| ref [ 3 ]; ref [ 3 ] |] in
  let tbl = Hashtbl.create (module Int) in
  Hashtbl.set tbl ~key:3 ~data:Int.Set.empty;

  let rec loop prev_p answer =
    let p = M.Prime.next_prime prev_p in

    if p >= answer
    then answer
    else (
      let idx = pred (p mod 3) in
      let desc_nbr_primes = get_pairable_primes p !(prime_lst.(idx)) answer in
      Hashtbl.set tbl ~key:p ~data:(Int.Set.of_list desc_nbr_primes);
      prime_lst.(idx) := p :: !(prime_lst.(idx));

      if List.length desc_nbr_primes < search_clique_size
      then loop p answer
      else (
        (* Search for cliques from p's neighbour primes *)
        match find_cliques desc_nbr_primes search_clique_size tbl with
        | [] -> loop p answer
        | cliques ->
          let min_sum_clique =
            List.(map cliques ~f:(reduce_exn ~f:( + )) |> min_elt ~compare:Int.compare)
            |> Option.value_exn
          in
          loop p (min answer (p + min_sum_clique))))
  in
  loop 5 Int.max_value
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 792
let%test_unit "5" = [%test_eq: int] (compute 5) 26033
