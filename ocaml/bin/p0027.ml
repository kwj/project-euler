(* Project Euler: Problem 27 *)

(*
  n^2 + an + b,  where abs(a) < 1000, abs(b) < 1000 and n=0...

  Find the product of the coefficients, 'a' and 'b', for the quadratic expression that
  produces the maximum number of x_primes for consecutive values of 'n', starting with 'n'=0.
 *)

(*
  when 'n' = 0:
    'b' must be a prime number. so, 1 < 'b' < 1000.
  when 'n' = 1:
    '1 + a + b' must be a prime number. Assume this prime number is 'x', 'a' = 'x' - 'b' - 1.
    Therefore, abs('x' - b - 1) < 1000 and 1 < 'b' < 1000, hence 0 < 'x' < 2000.
  when 'n' is odd number:
    'n^2 + b' is a even number. so 'a' must be a odd number.
  the value of 'b' cannot be 2:
    bacause value of the expression becomes even when 'n' is an even number. it's not a prime number.
 *)

open Core

let count_consecutive a b =
  let rec loop n =
    if Bool.equal (Euler.Math.mr_isprime (n * n + a * n + b)) false then n else loop (succ n)
  in
  loop 0

let solve () =
  let x_primes = Euler.Eratosthenes.to_list (Euler.Eratosthenes.generate 2_000) in    (* candidates of 'x' *)
  let b_primes = List.tl_exn (List.filter x_primes ~f:(fun n -> n < 1000)) in    (* candidates of 'b' *)
  let rec aux l max_len max_tpl =
    match l with
    | [] -> max_len, max_tpl
    | b :: tl ->
       let rec aux' a_lst max_len max_tpl =
         match a_lst with
         | [] -> max_len, max_tpl
         | a :: tl ->
            let len = count_consecutive a b in
            if len > max_len then
              aux' tl len (a, b)
            else
              aux' tl max_len max_tpl
       in
       let new_len, new_tpl =
         aux' (List.map ~f:(fun x -> x - b - 1) @@ List.filter x_primes ~f:(fun x -> (abs (x - b - 1)) < 1000)) max_len max_tpl in
       aux tl new_len new_tpl
  in
  aux b_primes 0 (0, 0)

let exec () =
  let len, (a, b) = solve () in
  sprintf "%d (a=%d, b=%d, length=%d)" (a * b) a b len

let () = Euler.Task.run exec
