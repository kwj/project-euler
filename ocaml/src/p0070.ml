(* Project Euler: Problem 70 *)

(*
 *  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.
 *
 * n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
 * -->
 *   phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
 *     <-->
 *   N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))
 *
 * From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
 * 11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...
 *
 * The answer N has the following form (p_i are prime numbers)
 *
 *   N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
 *)

open Core
module Prime = Euler.Math.Prime

let prod pf_lst =
  List.fold pf_lst ~init:1 ~f:(fun acc (base, exp) -> acc * Int.pow base exp)
;;

let phi pf_lst =
  List.fold pf_lst ~init:1 ~f:(fun acc (base, exp) ->
    acc * Int.pow base (pred exp) * pred base)
;;

let get_phi_ratio pf_lst = Float.(of_int (prod pf_lst) /. of_int (phi pf_lst))

(*
 * Note:
 *   The internal data 'pf_lst' has the following structure.
 *     [(p_n, e_n); ...; (p2, e2); (p1, e1)]
 *   In contrast, the function 'next' returns its reversed list.
 *     [(p1, e1); (p2, e2); ...; (p_n, e_n)]
 *)
let pf_generator tpl limit =
  let pf_lst =
    ref (if fst tpl = snd tpl then [ (fst tpl, 2) ] else [ (snd tpl, 1); (fst tpl, 1) ])
  in
  let aux pf_lst =
    let b, e = List.hd_exn pf_lst in
    let tmp = limit / prod pf_lst in
    if tmp < b
    then pf_lst
    else (
      let next_small_p = Prime.prev_prime (tmp + 1) in
      if next_small_p > b
      then (next_small_p, 1) :: pf_lst
      else (b, e + 1) :: List.tl_exn pf_lst)
  in
  let next () =
    let b, e = List.hd_exn !pf_lst in
    if List.length !pf_lst = 1 && e = 1
    then None (* [(_, 1)] ->  go to next smaller p_1 *)
    else (
      let result = List.rev !pf_lst in
      if e > 1
      then
        (* [(_, e); ...] --> [(_, e - 1); ...] *)
        pf_lst := (b, e - 1) :: List.tl_exn !pf_lst
      else (
        let b_2nd, e_2nd = List.nth_exn !pf_lst 1 in
        let next_small_p = Prime.prev_prime b in
        if next_small_p = b_2nd
        then
          (* [(_, 1); (p_{n-1}, e_{n-1}); ...] --> [(p_{n-1}, e_{n-1} + 1); ...] *)
          pf_lst := aux ((b_2nd, e_2nd + 1) :: List.tl_exn (List.tl_exn !pf_lst))
        else
          (* [(_, 1); (p_{n-1}, e_{n-1}); ...] --> [(prev_prime(p_{n}), 1); (p_{n-1}, e_{n-1}; ...] *)
          pf_lst := aux ((next_small_p, 1) :: List.tl_exn !pf_lst));
      Some result)
  in
  next
;;

let compute limit =
  let limit = limit - 1 in
  let module PQ =
    Pqueue.MakeMin (struct
      type t = float * (int * int) list

      let compare x y = Float.compare (fst x) (fst y)
    end)
  in
  let pq = PQ.create () in

  (* initial data for pruning: phi(87109) = 79180, 87109 = 11 * 7919 *)
  PQ.add pq (87109. /. 79180., [ (11, 1); (7919, 1) ]);

  let rec aux pf_gen =
    match pf_gen () with
    | None -> true (* go to next smaller prime *)
    | Some pf_lst ->
      if Float.(fst (PQ.get_min_elt pq) < get_phi_ratio (List.take pf_lst 2))
      then true (* pruning: skip to next smaller prime *)
      else (
        if Euler.Math.is_permutation (prod pf_lst) (phi pf_lst)
        then PQ.add pq (get_phi_ratio pf_lst, pf_lst);
        aux pf_gen)
  in
  ignore
    (Prime.primes 11 (Euler.Math.isqrt limit)
     |> List.rev_map ~f:(fun p -> (p, Prime.prev_prime ((limit / p) + 1)))
     |> List.for_all ~f:(fun tpl ->
       if Float.(get_phi_ratio [ (fst tpl, 1) ] > fst (PQ.get_min_elt pq))
       then false (* pruning: no further searching is needed. *)
       else aux (pf_generator tpl limit)));
  PQ.get_min_elt pq |> snd |> prod
;;

let solve () = compute 10_000_000 |> Int.to_string

(* Test *)

let%test_unit "10_000_000" = [%test_eq: int] (compute 10_000_000) 8319823
