(* Project Euler: Problem 50 *)

open Core

module E = Euler.Eratosthenes

let solve u_limit =
  let prime_t = E.generate u_limit in
  let check_consecutiveness lst =
    let rec aux l acc cnt result =
      match l with
      | x :: xs when (x + acc) <= u_limit ->
          if E.is_prime prime_t (x + acc) then
            aux xs (x + acc) (succ cnt) (cnt, x + acc)
          else
            aux xs (x + acc) (succ cnt) result
      | _ -> result
    in
    aux lst 0 1 (0, 0)
  in
  let rec make_prime_lst acc = function
    | x1 :: x2 :: _ when (x1 + x2) > u_limit -> List.rev (x1 :: acc)
    | x :: xs -> make_prime_lst (x :: acc) xs
    | [] -> List.rev acc
  in

  let prime_lst = make_prime_lst [] (E.to_list prime_t) in
  let rec aux lst (max_len, max_sum) =
    if List.length lst < max_len then
      max_len, max_sum
    else
      match lst with
      | _ :: xs -> let len, sum = check_consecutiveness lst in
                   if len > max_len then aux xs (len, sum) else aux xs (max_len, max_sum)
      | [] -> max_len, max_sum
  in
  aux prime_lst (0, 0)

let exec () =
  let len, sum = solve 1_000_000 in
  sprintf "%d (%d chains)" sum len

let () = Euler.Task.run exec
