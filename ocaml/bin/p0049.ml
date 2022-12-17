(* Project Euler: Problem 49 *)

open Core

module E = Euler.Eratosthenes

let prime_t = E.generate 9999

let rec find_tpls prime_lst result =
  let rec aux p1 lst acc =
    match lst with
    | x :: xs -> let p2 = (p1 + x) / 2 in
                 if E.is_prime prime_t p2 then
                   aux p1 xs ((p1, p2, x) :: acc)
                 else
                   aux p1 xs acc
    | [] -> acc
  in
  match prime_lst with
  | x :: xs -> find_tpls xs (aux x xs result)
  | [] -> result

let is_perm (p1, p2, p3) =
  let module M = Euler.Math in
  M.is_permutation p1 p2 && M.is_permutation p2 p3

let solve () =
  let p_lst = E.to_list prime_t |> List.filter ~f:(fun n -> n > 999) in
  List.filter (find_tpls p_lst []) ~f:is_perm
  |> List.filter ~f:(fun (p1, _, p3) -> p1 <> 1487 && p3 <> 8147)
  |> List.map ~f:(fun (p1, p2, p3) -> Int.to_string p1 ^ Int.to_string p2 ^ Int.to_string p3)

let exec () =
  String.concat ~sep:"\n" (solve ())

let () = Euler.Task.run exec
