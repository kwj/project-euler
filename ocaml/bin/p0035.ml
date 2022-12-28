(* Project Euler: Problem 35 *)

open Core

let solve num =
  let module E = Euler.Eratosthenes in
  let prime_t = E.generate(num) in
  let is_circular num =
    let k = Euler.Math.num_of_digits num in
    let d = Int.pow 10 (k - 1) in
    let rec aux num cnt result =
      if cnt = 0 || Bool.(result = false) then
        result
      else
        let next_num = (num mod 10) * d + (num / 10) in
        aux next_num (pred cnt) (E.is_prime prime_t next_num)
    in
    aux num (k - 1) true
  in
  let rec loop count = function
      x :: xs -> if Bool.(is_circular x = true) then
                   loop (succ count) xs
                 else
                   loop count xs
    | [] -> count
  in
  loop 0 (E.to_list prime_t)

let exec () =
  Int.to_string (solve 999_999)

let () = Euler.Task.run exec

