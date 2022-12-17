(* Project Euler: Problem 70 *)

(*
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
 *)

open Core

module E = Euler.Eratosthenes

let u_limit = 9_999_999

let prod pf_lst =
  List.fold pf_lst ~init:1 ~f:(fun acc (base, exp) -> acc * (Int.pow base exp))

let phi pf_lst =
  List.fold pf_lst ~init:1
    ~f:(fun acc (base, exp) -> acc * (Int.pow base (pred exp)) * (pred base))

let get_ratio pf_lst =
  Float.(of_int (prod pf_lst) /. of_int (phi pf_lst))

let pf_generator prime_t tpl =
  (*
    Note:
      The internal data 'pf_lst' has the following structure.
        [(p_n, e_n); ...; (p2, e2); (p1, e1)]
      In contrast, the function 'next' returns its reversed list.
        [(p1, e1); (p2, e2); ...; (p_n, e_n)]
   *)
  let pf_lst = ref (if fst tpl = snd tpl then
                      [(fst tpl, 2)]
                    else
                      [(fst tpl, 1); (snd tpl, 1)]) in
  let aux pf_lst =
    let base_n, exp_n = List.hd_exn pf_lst in
    let tmp = u_limit / (prod pf_lst) in
    if tmp < base_n then
      pf_lst
    else (
      let p_prime = E.prev_prime prime_t (tmp + 1) in
      if p_prime > base_n then
        (p_prime, 1) :: pf_lst
      else
        (base_n, (succ exp_n)) :: (List.tl_exn pf_lst)
    )
  in
  let next () =
    let base_n, exp_n = List.hd_exn !pf_lst in
    if List.length !pf_lst = 1 && exp_n = 1 then
      (* [(p_1, 1)] ->  go to next smaller p_1 *)
      None
    else (
      let result = List.rev !pf_lst in
      if exp_n > 1 then (
        (* [(p_n, e_n); ...] --> [(p_n, e_n - 1); ...] *)
        pf_lst := (base_n, (pred exp_n)) :: (List.tl_exn !pf_lst);
      ) else (
        let p_prime = E.prev_prime prime_t base_n and
            base_n', exp_n' = List.nth_exn !pf_lst 1 in
        if p_prime = base_n' then
          (* [(p_n, 1); (p_{n-1}, e_{n-1}); ...]
               --> [(p_{n-1}, e_{n-1} + 1); ...] *)
          pf_lst := aux ((base_n', (succ exp_n')) :: (List.tl_exn (List.tl_exn !pf_lst)))
        else
          (* [(p_n, 1); (p_{n-1}, e_{n-1}); ...]
               --> [(prev_prime(p_{n}), 1); (p_{n-1}, e_{n-1}; ...] *)
          pf_lst := aux ((p_prime, 1) :: (List.tl_exn !pf_lst))
      );
      Some result
    )
  in
  next

let solve prime_t tpl_lst =
  let module PQ = Euler.PrioQueue.Make(struct
                      type t = float * (int * int) list
                      let compare x y = Float.compare (fst y) (fst x)
                    end) in
  let pq = PQ.init () in

  (* initial data for pruning: phi(87109) = 79180, 87109 = 11 * 7919 *)
  PQ.insert pq (87109. /. 79180., [(11, 1); (7919, 1)]);

  let rec aux pf_gen =
    match pf_gen () with
      None -> true    (* go to next smaller p_1 *)
    | Some pf_lst ->
       if Float.(fst (PQ.peek pq) < get_ratio (List.take pf_lst 2)) then
         true    (* pruning: skip to next smaller p_1 *)
       else (
         if Euler.Math.is_permutation (prod pf_lst) (phi pf_lst) then (
           PQ.insert pq (get_ratio pf_lst, pf_lst)
         );
         aux pf_gen
       )
  in
  ignore (
    List.for_all tpl_lst
      ~f:(fun tpl ->
        let ratio = Float.((of_int (snd tpl) /. of_int (Int.((snd tpl) - 1)))) in
        if Float.(ratio > fst (PQ.peek pq)) then
          false  (* pruning: the end of search *)
        else
          aux (pf_generator prime_t tpl))
  );
  PQ.peek pq

let exec () =
  let prime_t = E.generate ((u_limit / 11) + 1) in
  let init_lst = E.to_list (E.generate (Euler.Math.isqrt u_limit))
                 |> List.drop_while ~f:(fun n -> n < 11)
                 |> List.rev_filter_map
                      ~f:(fun p1 -> let p2 = E.prev_prime prime_t ((u_limit / p1) + 1) in
                                    if p2 >= p1 then Some (p2, p1) else None)
  in
  let ratio, p_lst = solve prime_t init_lst in
  sprintf "%d (n/phi(n): %.10f)" (prod p_lst) ratio

let () = Euler.Task.run exec

