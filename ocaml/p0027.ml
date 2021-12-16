(* Project Euler: Problem 27 *)

(*
  n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

  Find the product of the coefficients, 'a' and 'b', for the quadratic expression that
  produces the maximum number of primes for consecutive values of 'n', starting with 'n'=0.
 *)

(*
  when 'n' = 0:
    'b' is a prime number. so, 1 < 'b' < 1000.
  when 'n' = 1:
    '1 + a + b' is a prime number. If this prime number is 'x', then 'a' = 'x' - 'b' - 1.
    abs('x' - b - 1) < 1000 and 1 < 'b' < 1000 ===> 0 < 'x' < 2000
  when 'n' is a odd number:
    'n^2 + b' is a even number. so 'a' must be a odd number.
  the value of 'b' cannot be 2:
    bacause value of the expression becomes even when 'n' is an even number. it's not a prime number.
 *)

let era_sieve n =
  let range order next a b =
    let rec aux a =
      if not (order a b) then [a] else a :: aux (next a)
    in
    aux a
  in
  let rec sieve lst =
    match lst with
    | [] -> []
    | hd :: tl -> hd :: (sieve (List.filter (fun x -> x mod hd <> 0) tl))
  in
  sieve (range (<) (fun x -> x + 1) 2 n)

let is_prime num =
  let upper = truncate @@ sqrt @@ float num in
  let rec aux n k =
    if k < 2 then true else (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

let count_consecutive a b =
  let rec loop n =
    if is_prime(n * n + a * n + b) = false then n else loop (succ n)
  in
  loop 0

let solve () =
  let primes = era_sieve 2000 in    (* candidates of 'x' *)
  let max_len, max_tupl = ref 0, ref (0, 0) in
  let rec aux l =
    match l with
    | [] -> !max_len, !max_tupl
    | b :: tl ->
       let a_list = List.map (fun x -> x - b - 1) @@ List.filter (fun x -> (abs (x - b - 1)) < 1000) primes in
       for i = 0 to (List.length a_list) - 1 do
         let a = List.nth a_list i in
         let len = count_consecutive a b in
         if len > !max_len then (
           max_len := len;
           max_tupl := (a, b)
         )
       done;
       aux tl
  in
  aux (List.tl (era_sieve 1000))

let () =
  let len, (a, b) = solve() in
  Printf.printf "the maximum number of primes for consecutive values of 'n', \
                 starting with 'n'=0 is %d (a=%d, b=%d -> a*b=%d)\n" len a b (a * b)
