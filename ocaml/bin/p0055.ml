(* Project Euler: Problem 55 *)

open Core

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let check_lychrel_z num =
  let is_palindromic n =
    let s = Z.to_string n in
    String.(s = rev s)
  in
  let rec aux n cnt =
    if cnt <= 1 then
      true
    else
      let next_n = Z.(n + of_string (String.rev (to_string n))) in
      if is_palindromic next_n then false else aux next_n (pred cnt)
  in
  aux (Z.of_int num) 50

let solve_1 () =
  let rec aux num cnt =
    if num < 1 then
      cnt
    else
      if check_lychrel_z num then
        aux (pred num) (succ cnt)
      else
        aux (pred num) cnt
  in
  aux (pred 10_000) 0

(* -- method 2 -- *)
(* using list instead of arbitrary-precision arithmetic library *)
let check_lychrel_l num =
  let module U = Euler.Util in
  let is_palindromic n_lst =
    List.equal Int.(=) n_lst (List.rev n_lst)
  in
  let rec aux n_lst cnt =
    if cnt <= 1 then
      true
    else
      let next_lst = U.add_nlst n_lst (List.rev n_lst) in
      if is_palindromic next_lst then false else aux next_lst (pred cnt)
  in
  aux (U.nlst_of_int num) 50

let solve_2 () =
  let rec aux num cnt =
    if num < 1 then
      cnt
    else
      if check_lychrel_l num then
        aux (pred num) (succ cnt)
      else
        aux (pred num) cnt
  in
  aux (pred 10_000) 0

let exec () =
  sprintf "%d: (w/ Zarith module)\n%d: (w/o Zarith module)" (solve_1 ()) (solve_2 ())

let () = Euler.Task.run exec
