(* Project Euler: Problem 60 *)

(*
  This program is slow. Here is a result on Raspberry Pi 4 Model B.

    % ./solve.sh 60
    26033
    Elapsed time: 25.453110933303833s
 *)

(*
A size-5 clique [8389, 6733, 5701, 5197, 13] is found at prime# = 8389.
The filter condition is	(5 - size of clique) * (current prime#) + (sum of clique) < 26033

prime# (num of size-5, size-4, size-3 and size-2 cliques)
-------------------------
2 (0, 0, 0, 0)
3 (0, 0, 0, 0)
5 (0, 0, 0, 0)
7 (0, 0, 0, 1)
11 (0, 0, 0, 2)
13 (0, 0, 0, 2)
  <snip>
8369 (0, 164, 6735, 13654)
8377 (0, 164, 6753, 13686)
8387 (0, 164, 6768, 13710)
8389 (1, 134, 3085, 379)
8419 (1, 134, 3047, 324)
8423 (1, 134, 3044, 315)
  <snip>
8647 (1, 141, 2781, 15)
8663 (1, 144, 2760, 8)
8669 (1, 144, 2755, 3)
8677 (1, 144, 2747, 0)
8681 (1, 146, 2745, 0)
  <snip>
12953 (1, 105, 3, 0)
12959 (1, 105, 1, 0)
12967 (1, 105, 0, 0)
12973 (1, 104, 0, 0)
  <snip>
25229 (1, 1, 0, 0)
25237 (1, 1, 0, 0)
25243 (1, 0, 0, 0)

*)
open Core

let update_cliques c_arr prime =
  let is_pair x y =
    let is_prime = Euler.Math.mr_isprime in
    let ndigits num =
      Float.(iround_down_exn (log10 (of_int num) + 1.))
    in
    is_prime (x * (Int.pow 10 (ndigits y)) + y) && is_prime (y * (Int.pow 10 (ndigits x)) + x)
  in
  let get_new_groups group_lst =
    (* group_lst: [(sum_of_primes, [primes]); ...] - (int * int list) list *)
    let new_groups = List.filter ~f:(fun (_, lst) -> List.for_all ~f:(fun n -> is_pair prime n) lst) group_lst
                     |> List.map ~f:(fun (sum, lst) -> (sum + prime, prime :: lst)) in
    if List.length new_groups = 0 then None else Some new_groups
  in
  let rec aux i =
    if i = 0 then
      if List.length c_arr.(5) = 0 then
        c_arr.(1) <- (prime, [prime]) :: c_arr.(1)
      else
        (* no need to search for more size-5 cliques which sum are larger than the sum of known size-5 clique *)
        ()
    else
      match get_new_groups c_arr.(i) with
      | None -> aux (pred i)
      | Some v -> c_arr.(i + 1) <- v @ c_arr.(i + 1);
                  aux (pred i)
  in
  aux 4

let prune_cliques c_arr cand_result current_prime =
  for i = 4 downto 2 do
    c_arr.(i) <- List.filter ~f:(fun (n, _) -> (5 - i) * current_prime + n < (fst cand_result)) c_arr.(i)
  done

let solve () =
  let p_gen = Euler.Math.Prime.generator () in
  let cliques = Array.create ~len:6 [] in    (* cliques.(0) is not used *)
  let is_end_of_search arr =
    List.length arr.(5) > 0 && List.length arr.(4) = 0 && List.length arr.(3) = 0 && List.length arr.(2) = 0
  in
  let rec loop result =
    if is_end_of_search cliques then
      result
    else (
      let prime = p_gen () in
      update_cliques cliques prime;
      if List.length cliques.(5) = 0 then (
        (* size-5 clique isn't found yet *)
        loop result
      ) else (
        cliques.(5) <- List.sort ~compare:(fun (x1, _) (x2, _) -> Int.compare x1 x2) cliques.(5);
        prune_cliques cliques (List.hd_exn cliques.(5)) prime;
        loop (List.hd_exn cliques.(5))
      )
    )
  in
  loop (0, [])

let exec () =
  Int.to_string (fst (solve ()))

let () = Euler.Task.run exec
