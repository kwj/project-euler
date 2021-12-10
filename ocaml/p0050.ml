(* Project Euler: Problem 50 *)

(* ---------------------------------------------------------------- *)

let make_primes n =
  (* Sieve of Eratosthenes *)
  let primes_array = Array.init (n + 1) (fun i -> i) in
  let primes_lst = ref [] in
  primes_array.(0) <- 0;
  primes_array.(1) <- 0;
  for i = 2 to n do
    if primes_array.(i) <> 0 then (
      if i <= (n / 2) then (
        primes_lst := i :: !primes_lst;
      );
      let j = ref (i * i) in
      while !j <= n do
        primes_array.(!j) <- 0;
        j := !j + i
      done
    )
  done;
  primes_array, !primes_lst

let find_maxlen p_arr p_lst =
  let max_len, max_sum = ref 0, ref 0 in
  let limit = Array.length p_arr - 1 in
  let rec aux lst cnt sum =
    match lst with
    | hd :: tl when hd + sum <= limit ->
       if p_arr.(hd + sum) <> 0 then (
         max_len := cnt + 1; max_sum := hd + sum
       );
       aux tl (succ cnt) (hd + sum)
    | _ ->
       !max_len, !max_sum
  in
  aux p_lst 0 0

let solve n =
  let p_arr, p_lst = make_primes n in
  let rec aux lst max_len max_sum =
    if List.length lst < max_len then
      max_len, max_sum
    else
      match lst with
      | hd :: tl ->
         let l, s = find_maxlen p_arr lst in
         if l > max_len then
           aux tl l s
         else
           aux tl max_len max_sum
      | [] -> max_len, max_sum
  in
  aux p_lst 0 0

let () =
  let len, sum = solve 1_000_000 in
  Printf.printf "Answer: len:%d, sum:%d\n" len sum
