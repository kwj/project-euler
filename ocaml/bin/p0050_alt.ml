(* Project Euler: Problem 50 [another version] *)

(* ---------------------------------------------------------------- *)

let make_primes n =
  (* Sieve of Eratosthenes *)
  let primes_array = Array.init (n + 1) (fun i -> i) in
  let primes_lst = ref [] in
  primes_array.(0) <- 0;
  primes_array.(1) <- 0;
  for i = 2 to n do
    if primes_array.(i) <> 0 then (
      primes_lst := i :: !primes_lst;
      let j = ref (i * i) in
      while !j <= n do
        primes_array.(!j) <- 0;
        j := !j + i
      done
    )
  done;
  List.rev !primes_lst

let rec drop lst n =
  match lst with
  | _ :: tl when n > 0 -> drop tl (pred n)
  | l -> l

let take_while lst f =
  let rec loop acc l =
    match l with
    | hd :: tl when f hd = true -> loop (hd :: acc) tl
    | _ -> List.rev acc
  in
  loop [] lst

let cu_sum lst =
  let rec aux pre acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (pre + hd) ((pre + hd) :: acc) tl
  in
  aux 0 [] lst

let find_maxlen prev lst p_lst =
  let rec loop l =
    match l with
    | hd :: tl when List.mem (hd - prev) p_lst -> l
    | hd :: tl -> loop tl
    | [] -> []
  in
  loop (List.rev lst)

let solve limit =
  let trim_plst lst =
    let rec loop = function
      | a :: (b :: _ as tl) when a + b >= limit -> loop tl
      | l -> List.rev l
    in
    loop (List.rev lst)
  in
  let primes_lst = make_primes limit in
  let cusum_plst = cu_sum (trim_plst primes_lst) in
  let rec loop lst prev max_len acc =
    if List.length lst < max_len || (max_len > 0 && (List.nth lst (max_len - 1)) - prev > limit) then
      max_len, acc
    else
      let tgt_lst = find_maxlen prev (drop (take_while lst (fun n -> n - prev < limit)) max_len) primes_lst in
      if tgt_lst = [] then
        loop (List.tl lst) (List.hd lst) max_len acc
      else
        loop (List.tl lst) (List.hd lst) (max_len + (List.length tgt_lst)) (List.hd tgt_lst - prev)
  in
  loop cusum_plst 0 0 0

let () =
  let len, sum = solve 1_000_000 in
  Printf.printf "Answer: len:%d, sum:%d\n" len sum
