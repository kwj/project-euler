(* Project Euler: Problem 51 *)

(*
  the smallest prime which, by replacing part of the number with same digit,
  is part of an eight prime value family

  -> eight numbers out of '0' to '9' are used for replacement

  1) the last digit is not eligible for replacement
    It make some even numbers after replacement.

  2) the number of digits of the prime numbers is greater than number of digits to be replaced
    the reason is since 1).

  3) the number of digits that can be replaced is only a multiples of 3
    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

      number  digits  sum  'mod 3'    [n>0]
      ---------------------------------------------
      0       n       0    0
      1       n       n    n mod 3
      2       n       2n   2n mod 3
      3       n       3n   3n mod 3 = 0
      4       n       4n   4n mod 3 = n mod 3
      5       n       5n   5n mod 3 = 2n mod 3
      6       n       6n   6n mod 3 = 0
      7       n       7n   7n mod 3 = n mod 3
      8       n       8n   8n mod 3 = 2n mod 3
      9       n       9n   9n mod 3 = 0

  I want to use prime number tables to speed up and simplify. For the
  moment, I assume that prime numbers less than one million. The reason
  is that I want to only consider the case of replacing three digits.

   4) There are at least same three numbers other than last digit.

 *)

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
  primes_array, List.rev !primes_lst

let digits_of_int num =
  let rec aux n =
    if n < 10 then [n] else (n mod 10) :: aux (n / 10)
  in
  List.rev (aux num)

let rec zip xs ys =
  match xs, ys with
  | x :: xs, y :: ys -> (x, y) :: (zip xs ys)
  | _, _ -> []

let compare_pat num pat =
  let rec shrink lst =
    match lst with
    | a :: (b :: _ as tl) when a = b -> shrink tl
    | hd :: tl -> hd :: (shrink tl)
    | [] -> []
  in
  let kinds = zip pat (digits_of_int num)
              |> List.filter (fun (x, _) -> x = 1)
              |> List.sort compare
              |> shrink
              |> List.length
  in
  if kinds = 1 then true else false

let choose_patlst num =
  let d4 = [
      [1; 1; 1; 0]
    ] in
  let d5 = [
      [0; 1; 1; 1; 0];
      [1; 0; 1; 1; 0];
      [1; 1; 0; 1; 0];
      [1; 1; 1; 0; 0]
    ] in
  let d6 = [
      [0; 0; 1; 1; 1; 0];
      [0; 1; 0; 1; 1; 0];
      [0; 1; 1; 0; 1; 0];
      [0; 1; 1; 1; 0; 0];
      [1; 0; 0; 1; 1; 0];
      [1; 0; 1; 0; 1; 0];
      [1; 0; 1; 1; 0; 0];
      [1; 1; 0; 0; 1; 0];
      [1; 1; 0; 1; 0; 0];
      [1; 1; 1; 0; 0; 0]
    ] in
  match List.length (digits_of_int num) with
  | 4 -> List.filter (compare_pat num) d4
  | 5 -> List.filter (compare_pat num) d5
  | 6 -> List.filter (compare_pat num) d6
  | _ -> assert false

let pruning num =
  let cnt_table = Array.make 10 0 in
  let rec aux n =
    if n = 0 then
      Array.exists (fun n -> n >= 3) cnt_table
    else (
      cnt_table.(n mod 10) <- cnt_table.(n mod 10) + 1;
      aux (n / 10)
    )
  in
  aux (num / 10)

let count_keys htbl lst =
  let rec aux lst =
    match lst with
    | (key, prime) :: tl ->
       if Hashtbl.mem htbl key then (
         Hashtbl.replace htbl key (prime :: (Hashtbl.find htbl key))
       ) else (
         Hashtbl.add htbl key [prime]
       );
       aux tl
    | _ -> htbl
  in
  aux lst

let solve num =
  let family_tbl = Hashtbl.create 1024 in    (* 1024 is a tentative value, meaningless *)
  let _, tmp_lst = make_primes num in
  let p_lst = List.filter ((<) 999) tmp_lst |> List.filter (pruning) in
  let rec aux lst result =
    match lst with
    | hd :: tl ->
       let pats = choose_patlst hd in
       if pats = [] then
         aux tl result
       else
         aux tl (List.map (fun pat ->
                     List.map2 (fun n p -> if p = 0 then string_of_int n else "#")
                       (digits_of_int hd) pat
                     |> List.fold_left (fun s1 s2 -> s1 ^ s2) "", hd)
                   pats @ result)
    | [] -> result
  in
  Hashtbl.to_seq_values (count_keys family_tbl (aux p_lst []));
  |> List.of_seq
  |> List.filter (fun e -> List.length e = 8)
  |> List.hd

let () =
  Printf.printf "Answer: %d\n" (List.hd (solve 1_000_000))
