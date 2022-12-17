(* Project Euler: Problem 70 *)

(* Check n/phi(n) in ascending order *)

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

let solve () =
  let module PQ = Euler.PrioQueue.Make(struct
                      type t = float * (int * int) list
                      let compare x y = Float.compare (fst y) (fst x)
                    end) in
  let pq = PQ.init () in
  let prime_t = E.generate ((u_limit / 11) + 1) in

  let get_pf () =
    let next_pf pf_lst =
      let aux pf_lst =
        let base_n, exp_n = List.hd_exn pf_lst in
        let tmp = u_limit / (prod pf_lst) in
        if tmp < base_n then
          [pf_lst]
        else (
          let p_prime = E.prev_prime prime_t (tmp + 1) in
          if p_prime > base_n then
            [pf_lst; (p_prime, 1) :: pf_lst]
          else
            [pf_lst; (base_n, (succ exp_n)) :: (List.tl_exn pf_lst)]
        )
      in
      let base_n, exp_n = List.hd_exn pf_lst in
      if List.length pf_lst = 1 && exp_n = 2 then
        None
      else (
        if exp_n > 1 then
          Some [(base_n, (pred exp_n)) :: (List.tl_exn pf_lst)]
        else
          let p_prime = E.prev_prime prime_t base_n and
              base_n', exp_n' = List.nth_exn pf_lst 1 in
          match Int.compare p_prime base_n' with
            0 -> Some (aux ((base_n', (succ exp_n')) :: (List.tl_exn (List.tl_exn pf_lst))))
          | n when n > 0 -> Some (aux ((p_prime, 1) :: (List.tl_exn pf_lst)))
          | _ -> None
      )
    in
    try
      let result = PQ.extract pq in
      match next_pf (snd result) with
        None -> Some result
      | Some llst -> List.iter llst ~f:(fun lst -> PQ.insert pq (get_ratio lst, lst));
                     Some result
    with
      Caml.Not_found -> None
  in
  let rec aux () =
    match get_pf () with
      None -> None
    | Some tpl -> if Euler.Math.is_permutation (prod (snd tpl)) (phi (snd tpl)) then
                    Some tpl
                  else
                    aux ()
  in
  List.iter (List.drop_while (E.to_list (E.generate (Euler.Math.isqrt u_limit))) ~f:(fun n -> n < 11))
    ~f:(fun p1 -> let p2 = E.prev_prime prime_t ((u_limit / p1) + 1) in
                  if p2 > p1 then
                    PQ.insert pq (get_ratio [(p2, 1); (p1, 1)], [(p2, 1); (p1, 1)])
                  else
                    PQ.insert pq (get_ratio [(p1, 2)], [(p1, 2)]));
  aux ()

let exec () =
  match solve () with
    None -> "Not Found"
  | Some tpl -> sprintf "%d (n/phi(n): %.10f)" (prod (snd tpl)) (fst tpl)
  
let () = Euler.Task.run exec

