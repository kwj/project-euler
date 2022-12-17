(* Project Euler: Problem 48 *)

(*
  >>> math.log((10**10 - 1) * (10**10 - 1), 2)
  66.43856189745871

  so, I use the Zarith module. However I don't use Z.powm because it's not fun.

  Here is an information about modular exponentiation.
    - https://en.wikipedia.org/wiki/Modular_exponentiation
 *)

open Core

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let mod_pow base exp modulus =
  let open Z in
  let m = of_int modulus in
  let rec aux b e result =
    if Int.(e <= 0) then
      to_int result
    else
      if Int.(rem e 2 = 1) then
        aux ((b * b) mod m) Int.(e / 2) ((result * b) mod m)
      else
        aux ((b * b) mod m) Int.(e / 2) result
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
  let module U = Euler.Util in
  let rec loop_a a acc =
    if a < 0 then
      List.take acc 10
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
            loop_b (List.take (U.mul_nlst lst a) 10) (pred cnt)
        in
        loop_a (pred a) (List.take (U.add_nlst (loop_b (U.nlst_of_int a) a) acc) 10)
  in
  List.fold ~f:(^) ~init:"" (List.map ~f:Int.to_string (List.rev (loop_a num [])))

let exec () =
  sprintf "%d  (w/ Zarith module)\n%s  (w/o Zarith module)" (solve_1 1000) (solve_2 1000)

let () = Euler.Task.run exec
