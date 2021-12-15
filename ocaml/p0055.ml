(* Project Euler: Problem 55 *)

(* ---------------------------------------------------------------- *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let check_lychrel_z num =
  let is_palindromic n =
    let s = Z.to_string n in
    let len = String.length s in
    let rec aux i =
      if (2 * i <= len) then
        true
      else
        if s.[len - i] <> s.[i - 1] then
          false
        else
          aux (pred i)
    in
    aux len
  in
  let rec aux n cnt =
    if cnt <= 1 then
      true
    else
      if (cnt <> 50 && is_palindromic n = true) then
        false
      else
        let tmp = Z.to_string n in
        let rev_s = String.init (String.length tmp) (fun i -> tmp.[String.length tmp - 1 - i]) in
        aux Z.(n + of_string rev_s) (pred cnt)
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
  let is_palindromic n_lst =
    if n_lst = List.rev n_lst then true else false
  in
  let rec add_nlst l1 l2 =
    let rec aux l1 l2 carry acc =
      match l1, l2 with
      | hd1 :: tl1, hd2 :: tl2 ->
         aux tl1 tl2 ((hd1 + hd2 + carry) / 10) (((hd1 + hd2 + carry) mod 10) :: acc)
      | [], [] when carry > 0 ->
         List.rev (carry :: acc)
      | _ ->
         List.rev acc
    in
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      aux l1 l2 0 []
    else
      if diff > 0 then
        add_nlst l1 (l2 @ [0])
      else
        add_nlst (l1 @ [0]) l2
  in
  let lst_of_int num =
    let rec aux n acc =
      if n = 0 then
        acc
      else
        aux (n / 10) ((n mod 10) :: acc)
    in
    List.rev (aux num [])
  in
  let rec aux n_lst cnt =
    if cnt <= 1 then
      true
    else
      if (cnt <> 50 && is_palindromic n_lst = true) then
        false
      else
        aux (add_nlst n_lst (List.rev n_lst)) (pred cnt)
  in
  aux (lst_of_int num) 50

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

let () =
  Printf.printf "Answer:\n";
  Printf.printf "   %d: (w/ Zarith module)\n" (solve_1());
  Printf.printf "   %d: (w/o Zarith module)\n" (solve_2())
