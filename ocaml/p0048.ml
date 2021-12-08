(* Project Euler: Problem 48 *)

(*
  >>> math.log((10**10 - 1) * (10**10 - 1), 2)
  66.43856189745871

  so, I use the Zarith module. However I don't use Z.powm because it's not fun.

  Here is an information about modular exponentiation.
    - https://en.wikipedia.org/wiki/Modular_exponentiation
 *)

(* ---------------------------------------------------------------- *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let mod_pow base exp modulus =
  let open Z in
  let m = of_int modulus in
  let rec aux b e result =
    if e <= 0 then
      to_int result
    else
      if Int.rem e 2 = 1 then
        aux ((b * b) mod m) (Int.div e 2) ((result * b) mod m)
      else
        aux ((b * b) mod m) (Int.div e 2) result
  in
  aux ((of_int base) mod m) exp (of_int 1)

let solve_1 num =
  let modulus = 10_000_000_000 in
  let rec aux n result =
    if n = 0 then
      result
    else
      aux (pred n) ((result + (mod_pow n n modulus)) mod modulus)
  in
  aux num 0

(* -- method 2 -- *)
(* using list, but it's a straightforward calculation *)
let solve_2 num =
  let lst_of_int num =
    let rec aux n acc =
      if n = 0 then
        List.rev acc
      else
        aux (n / 10) ((n mod 10) :: acc)
    in
    aux num []
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
  let mul_nlst lst num =
    let rec aux lst carry acc =
      match lst with
      | hd :: tl ->
         aux tl ((hd * num + carry) / 10) (((hd * num + carry) mod 10) :: acc)
      | [] when carry > 0 ->
         aux [] (carry / 10) ((carry mod 10) :: acc)
      | _ ->
         List.rev acc
    in
    aux lst 0 []
  in
  let take n lst =
    let rec aux m l acc =
      if m = 0 || l = [] then
        List.rev acc
      else
        aux (pred m) (List.tl l) (List.hd l :: acc)
    in
    aux n lst []
  in
  let rec loop_a a acc =
    if a < 0 then
      take 10 acc
    else
      if a mod 10 = 0 then
        (* no calculation is required since the last 10 digits are all zeros *)
        loop_a (pred a) acc
      else
        let rec loop_b lst cnt =
          if cnt = 1 then
            lst
          else
            (* we are only interested in the last 10 digits *)
            loop_b (take 10 (mul_nlst lst a)) (pred cnt)
        in
        loop_a (pred a) (take 10 (add_nlst (loop_b (lst_of_int a) a) acc))
  in
  List.fold_left (^) "" (List.map string_of_int (List.rev (loop_a num [])))

let () =
  Printf.printf "Answer: %d  (w/ Zarith module)\n" (solve_1 1000);
  Printf.printf "Answer: %s  (w/o Zarith module)\n" (solve_2 1000)
