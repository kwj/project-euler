(* Project Euler: Problem 97 *)

(*
  >>> math.log((10**10 - 1) * (10**10 - 1), 2)
  66.43856189745871

  so, I use the Zarith module. However I don't use Z.powm because it's not fun.

  Here is an information about modular exponentiation.
    - https://en.wikipedia.org/wiki/Modular_exponentiation

  Note: Similar to problem 48.
  Note 2: Without Zarith module version is omitted since I'm tired.
 *)

(* ---------------------------------------------------------------- *)

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

let solve () =
  let modulus = 10_000_000_000 in
  (28433 * (mod_pow 2 7830457 modulus) + 1) mod modulus


let () =
  Printf.printf "Answer: %d  (w/ Zarith module)\n" (solve ())
