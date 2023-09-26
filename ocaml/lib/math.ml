(* Various functions *)

let rec gcd m n =
  (* Euclidean algorithm *)
  if n = 0 then abs m else gcd n (m mod n)
;;

let lcm m n =
  match (m, n) with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / gcd m n
;;

let binomial n k =
  Seq.zip (Seq.ints (n - k + 1)) (Seq.ints 1)
  |> Seq.take k
  |> Seq.fold_left (fun acc (numer, denom) -> acc * numer / denom) 1
;;

let rec int_pow b e =
  match e with
  | 0 -> 1
  | 1 -> b
  | n ->
    let x = int_pow b (e / 2) in
    x * x * if n mod 2 = 0 then 1 else b
;;

let powmod base exp modulus =
  let m = Z.(powm (of_int base) (of_int exp) (of_int modulus)) in
  Z.to_int m
;;

let get_max_exp ?(base = 10) num =
  let rec aux n cnt = if n < base then cnt else aux (n / base) (succ cnt) in
  aux num 0
;;

let num_of_digits ?(base = 10) num = get_max_exp num ~base + 1

(* Integer Square Root *)
(*
 * https://github.com/mdickinson/snippets/blob/master/proofs/isqrt/src/isqrt.lean
 *
 * def isqrt_aux(c, n):
 *     if c == 0:
 *         return 1
 *     else:
 *         k = (c - 1) // 2
 *         a = isqrt_aux(c // 2, n >> 2*k + 2)
 *         return (a << k) + (n >> k+2) // a
 *
 * def isqrt(n):
 *     if n == 0:
 *         return 0
 *     else:
 *         a = isqrt_aux((n.bit_length() - 1) // 2, n)
 *         return a - 1 if n < a * a else a
 *)
let _bit_length num =
  if num = 0
  then 0
  else (
    let rec aux n cnt = if n = 0 then cnt else aux (n lsr 1) (succ cnt) in
    aux num 0)
;;

let isqrt num =
  let rec aux c n =
    if c = 0
    then 1
    else (
      let k = (c - 1) / 2 in
      let a = aux (c / 2) (n lsr ((2 * k) + 2)) in
      (a lsl k) + ((n lsr (k + 2)) / a))
  in
  if num = 0
  then 0
  else (
    let a = aux ((_bit_length num - 1) / 2) num in
    if num < a * a then a - 1 else a)
;;

(* ---- From here on, the following functions can be used within this module ---- *)
(*  gcd, lcm, binomial, int_pow, powmod *)
(*  get_max_exp, num_of_digits, isqrt *)

(* Divisor and Prime factorization *)
let divisors n =
  let u_limit = isqrt n in
  let rec aux lst i =
    if i > u_limit
    then lst
    else if n mod i <> 0
    then aux lst (succ i)
    else if i * i = n
    then aux (i :: lst) (succ i)
    else aux ((n / i) :: i :: lst) (succ i)
  in
  List.sort compare (aux [] 1)
;;

(* prime factorization *)
(*   example: 126 -> [(2, 1); (3, 2); (7, 1)] *)
let _div_loop num b =
  let n = ref num in
  let e = ref 0 in
  while !n mod b = 0 do
    e := !e + 1;
    n := !n / b
  done;
  if !e <> 0 then Some (!n, (b, !e)) else None
;;

let _div_235 num =
  let rec aux n res = function
    | [] -> (n, res)
    | b :: bs ->
      (match _div_loop n b with
       | Some (next_n, tpl) -> aux next_n (tpl :: res) bs
       | None -> aux n res bs)
  in
  aux num [] [ 2; 3; 5 ]
;;

let factorize num =
  assert (num > 0);
  if num = 1
  then [ (1, 1) ]
  else (
    let n, res = _div_235 num in
    let limit = isqrt n in
    let diff = [| 4; 2; 4; 2; 4; 6; 2; 6 |] in
    let rec aux n b idx res =
      if b > limit
      then (n, res)
      else (
        match _div_loop n b with
        | Some (next_n, tpl) ->
          aux next_n (b + diff.(idx)) ((idx + 1) mod Array.length diff) (tpl :: res)
        | None -> aux n (b + diff.(idx)) ((idx + 1) mod Array.length diff) res)
    in
    let rest_n, lst = aux n 7 0 res in
    if rest_n <> 1 then List.rev ((rest_n, 1) :: lst) else List.rev lst)
;;

let pfactors_to_divisors pf_lst =
  let rec aux divs (base, exp) acc =
    if exp = 0
    then acc
    else aux divs (base, pred exp) (List.map (fun n -> n * int_pow base exp) divs @ acc)
  in
  let rec loop_factor lst result =
    match lst with
    | (b, e) :: xs -> loop_factor xs (aux result (b, e) result)
    | [] -> result
  in
  loop_factor pf_lst [ 1 ] |> List.sort compare
;;

(* Polygonal number test *)
let is_triangular num =
  let tmp = (8 * num) + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 2 = 1
;;

let is_square num =
  let num_sqrt = isqrt num in
  num = num_sqrt * num_sqrt
;;

let is_pentagonal num =
  let tmp = (24 * num) + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 6 = 5
;;

let is_hexagonal num =
  let tmp = (8 * num) + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 4 = 3
;;

(* pandigital number test *)
let is_pandigital num =
  let mk_bits num =
    let rec aux n bits =
      if n = 0 then bits else aux (n / 10) (bits lor (1 lsl (n mod 10)))
    in
    aux num 0
  in
  mk_bits num = (1 lsl num_of_digits num) - 1
;;

let is_pandigital_nz num =
  let rec check_zero n =
    if n = 0 then true else if n mod 10 = 0 then false else check_zero (n / 10)
  in
  num > 0 && check_zero num && is_pandigital (num * 10)
;;

(* palindrome number test*)
let is_palindrome ?(base = 10) num =
  let rec loop n acc =
    if n = 0 then acc else loop (n / base) ((acc * base) + (n mod base))
  in
  num = loop num 0
;;

(* Comparison by sorting figures *)
let is_permutation x y =
  let tbl = Array.init 10 (fun _ -> 0) in
  let count fn n =
    let rec aux i =
      if i <> 0
      then (
        tbl.(i mod 10) <- fn tbl.(i mod 10) 1;
        aux (i / 10))
      else ()
    in
    aux n
  in
  count ( + ) x;
  count ( - ) y;
  Array.for_all (fun i -> i = 0) tbl
;;

(*
 * Functions related to prime numbers
 *)

module Prime = struct
  (* Primality test by naive method *)
  let is_prime num =
    let upper = isqrt num in
    let rec aux n k = if k < 2 then true else n mod k <> 0 && aux n (k - 1) in
    if num <= 1 then false else aux num upper
  ;;

  (* Miller–Rabin primality test (Wikipedia based deterministic version under 2^64) *)
  (*   [Miller–Rabin primality test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test) *)
  (*   [Deterministic variants of the Miller-Rabin primality test](http://miller-rabin.appspot.com/) *)
  type typenum =
    | Composite
    | Prime
    | Undecided

  let mr_isprime num =
    if num < 2 || (num mod 6 <> 1 && num mod 6 <> 5)
    then num = 2 || num = 3
    else (
      let init_value n =
        let rec aux s d = if d mod 2 <> 0 then (s, d) else aux (succ s) (d / 2) in
        aux 0 (pred n)
      in
      let distinguish a d s n =
        if a mod n = 0
        then Prime
        else (
          let rec loop x cnt =
            if cnt = 0
            then if x <> 1 then Composite else Undecided
            else (
              let y = powmod x 2 n in
              if y = 1 && x <> 1 && x <> n - 1 then Composite else loop y (pred cnt))
          in
          loop (powmod a d n) s)
      in
      (* 'num' is not a multiple of 2 or 3 from here *)
      let s, d = init_value num in
      let rec sprp_loop = function
        | [] -> true
        | x :: xs ->
          (match distinguish x d s num with
           | Prime -> true
           | Composite -> false
           | Undecided -> sprp_loop xs)
      in

      (* Jim Sinclair's bases *)
      sprp_loop [ 2; 325; 9375; 28178; 450775; 9780504; 1795265022 ])
  ;;

  (*
   * Wheel index
   *     ..=7:  0
   *   8..=11: 1
   *   12..=13: 2
   *   14..=17: 3
   *   18..=19: 4
   *   20..=23: 5
   *   24..=29: 6
   *   30..=31: 7
   *   32..=37: 8 + 0
   *   38..=41: 8 + 1
   *   .....
   *)

  let num_to_idx n =
    let indices =
      [| 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 3; 3; 3; 3; 4; 4; 5; 5; 5; 5; 6; 6; 6; 6; 6; 6; 7; 7 |] [@ocamlformat "disable"]
    in
    if n < 2
    then 0
    else (
      let q = (n - 2) / 30 in
      let r = (n - 2) mod 30 in
      (8 * q) + indices.(r))
  ;;

  let idx_to_num idx =
    let wheel = [| 7; 11; 13; 17; 19; 23; 29; 31 |] in
    let q = idx / 8 in
    let r = idx mod 8 in
    (30 * q) + wheel.(r)
  ;;

  let next_prime n =
    match n with
    | n when n < 2 -> 2
    | n when n < 3 -> 3
    | n when n < 5 -> 5
    | n when n < 7 -> 7
    | n ->
      let rec aux idx =
        let x = idx_to_num idx in
        if is_prime x then x else aux (succ idx)
      in
      aux (num_to_idx n + 1)
  ;;

  let prev_prime n =
    match n with
    | n when n <= 3 -> 2
    | n when n <= 5 -> 3
    | n when n <= 7 -> 5
    | n ->
      let rec aux idx =
        let x = idx_to_num idx in
        if is_prime x then x else aux (pred idx)
      in
      aux (num_to_idx n - 1)
  ;;

  (*
     Note: Multiples of 2, 3 or 5 are omitted.
          Primes less than or equal to 30 are [1, 7, 11, 13, 17, 19, 23, 29].

     // reminders divided by 30
     let mod30 = [|1; 7; 11; 13; 17; 19; 23; 29|]

     prime: p = 30q + r

       p * p : start (cmposite)
       p * (p + D) : p + D is other than mulitles of 2, 3 or 5
         ...
       example: p = 7
         7 * 7, 7 * 11, 7 * 13, 7 * 17, 7 * 19, 7 * 23, 7 * 29, 7 * 31, ...

     prime table:
       byte 0: 1-30
       byte 1: 31-60
         ...

         byte q: MSB    LSB
                 xxxxxxxx   // r_bit/b_bit: 0(LSB) .. 7(MSB)
                 ||||||||
                 |||||||+-- 30q + 1
                 ||||||+--- 30q + 7
                 |||||+---- 30q + 11
                 ||||+----- 30q + 13
                 |||+------ 30q + 17
                 ||+------- 30q + 19
                 |+-------- 30q + 23
                 +--------- 30q + 29

     // start : p * p = p^2
     p^2 = (30q + r) * (30q + r)
         = 900q^2 + 60qr + r^2

       // byte position (index for prime table)
       p^2/30 = 30q^2 + 2qr + r^2/30
             = q(30q + 2r) + r^2/30
               ^^^^^^^^^^^^^^^^^^^^ [*1]
       // bit position information
       p^2 % 30 = r^2 % 30
                 ^^^^^^^^ [*2]
     // D: diff
     p * k{i} = (30q + r) * (30a + b)
     p * k{i+1} = (30q + r) * (30a + b + D)
               = (30q + r) * (30a + c)

       // offset of byte position
       k{i+1}*p/30 - k{i}*p/30 = ((30a + c) * (30q + r))/30 - ((30a + b) * (30q + r))/30
                               = (900aq + 30ar + 30cq + cr)/30 - (900aq + 30ar + 30bq + br)/30
                               = q(c-b) + cr/30 - br/30
                                         ^^^^^^^^^^^^^ [*3]
                                 ^^^^^^^^^^^^^^^^^^^^^^ [*4]
       // bit position information
       (k{i+1} * p) % 30 = cr % 30
                           ^^^^^^^ [*2]

     // [*3] c-b: difference of elements of mod30
     let mod30_diff = [|6; 4; 2; 4; 2; 4; 6; 2|]

       c -> 7; 11; 13; 17; 19; 23; 29; 31
       b -> 1;  7; 11; 13; 17; 19; 23; 29
       -----------------------------------
           6;  4;  2;  4;  2;  4;  6;  2

     // [*4] cr/30 - br/30 : rb_tbl
     let rb_tbl = [|[|0; 0; 0; 0; 0; 0; 0; 1|]; [|1; 1; 1; 0; 1; 1; 1; 1|];
                   [|2; 2; 0; 2; 0; 2; 2; 1|]; [|3; 1; 1; 2; 1; 1; 3; 1|];
                   [|3; 3; 1; 2; 1; 3; 3; 1|]; [|4; 2; 2; 2; 2; 2; 4; 1|];
                   [|5; 3; 1; 4; 1; 3; 5; 1|]; [|6; 4; 2; 4; 2; 4; 6; 1|]|]

       rem = [1,7,11,13,17,19,23,29]
       c = {1:7, 7:11, 11:13, 13:17, 17:19, 19:23, 23:29, 29:31}
       >>> [[(c[b] * r) // 30 - (b * r) // 30 for b in rem] for r in rem]
       [[0, 0, 0, 0, 0, 0, 0, 1], [1, 1, 1, 0, 1, 1, 1, 1],
       [2, 2, 0, 2, 0, 2, 2, 1], [3, 1, 1, 2, 1, 1, 3, 1],
       [3, 3, 1, 2, 1, 3, 3, 1], [4, 2, 2, 2, 2, 2, 4, 1],
       [5, 3, 1, 4, 1, 3, 5, 1], [6, 4, 2, 4, 2, 4, 6, 1]]

     // [*2] mask data
     let mask_tbl = [|[|0xFE; 0xFD; 0xFB; 0xF7; 0xEF; 0xDF; 0xBF; 0x7F|];
                      [|0xFD; 0xDF; 0xEF; 0xFE; 0x7F; 0xF7; 0xFB; 0xBF|];
                      [|0xFB; 0xEF; 0xFE; 0xBF; 0xFD; 0x7F; 0xF7; 0xDF|];
                      [|0xF7; 0xFE; 0xBF; 0xDF; 0xFB; 0xFD; 0x7F; 0xEF|];
                      [|0xEF; 0x7F; 0xFD; 0xFB; 0xDF; 0xBF; 0xFE; 0xF7|];
                      [|0xDF; 0xF7; 0x7F; 0xFD; 0xBF; 0xFE; 0xEF; 0xFB|];
                      [|0xBF; 0xFB; 0xF7; 0x7F; 0xFE; 0xEF; 0xDF; 0xFD|];
                      [|0x7F; 0xBF; 0xDF; 0xEF; 0xF7; 0xFB; 0xFD; 0xFE|]|]

       rem = [1,7,11,13,17,19,23,29]
       m = {1:0xFE, 7:0xFD, 11:0xFB, 13:0xF7, 17:0xEF, 19:0xDF, 23:0xBF, 29:0x7F}
       >>> [[hex(m[(x * y) % 30]) for y in rem] for x in rem]
       [['0xfe', '0xfd', '0xfb', '0xf7', '0xef', '0xdf', '0xbf', '0x7f'],
        ['0xfd', '0xdf', '0xef', '0xfe', '0x7f', '0xf7', '0xfb', '0xbf'],
        ['0xfb', '0xef', '0xfe', '0xbf', '0xfd', '0x7f', '0xf7', '0xdf'],
        ['0xf7', '0xfe', '0xbf', '0xdf', '0xfb', '0xfd', '0x7f', '0xef'],
        ['0xef', '0x7f', '0xfd', '0xfb', '0xdf', '0xbf', '0xfe', '0xf7'],
        ['0xdf', '0xf7', '0x7f', '0xfd', '0xbf', '0xfe', '0xef', '0xfb'],
        ['0xbf', '0xfb', '0xf7', '0x7f', '0xfe', '0xef', '0xdf', '0xfd'],
        ['0x7f', '0xbf', '0xdf', '0xef', '0xf7', '0xfb', '0xfd', '0xfe']]
  *)

  (*
     type t = {
      size: int;
      data: char array
     }
  *)

  (* [*3] *)
  let rb_tbl =
    [| [| 0; 0; 0; 0; 0; 0; 0; 1 |]
     ; [| 1; 1; 1; 0; 1; 1; 1; 1 |]
     ; [| 2; 2; 0; 2; 0; 2; 2; 1 |]
     ; [| 3; 1; 1; 2; 1; 1; 3; 1 |]
     ; [| 3; 3; 1; 2; 1; 3; 3; 1 |]
     ; [| 4; 2; 2; 2; 2; 2; 4; 1 |]
     ; [| 5; 3; 1; 4; 1; 3; 5; 1 |]
     ; [| 6; 4; 2; 4; 2; 4; 6; 1 |]
    |]
  ;;

  (* [*4] *)
  let mod30 = [| 1; 7; 11; 13; 17; 19; 23; 29 |]
  let mod30_diff = [| 6; 4; 2; 4; 2; 4; 6; 2 |]
  let get_idx_offset q r b = (q * mod30_diff.(b)) + rb_tbl.(r).(b)

  (* [*2] *)
  let mask_tbl =
    [| [| 0xFE; 0xFD; 0xFB; 0xF7; 0xEF; 0xDF; 0xBF; 0x7F |]
     ; [| 0xFD; 0xDF; 0xEF; 0xFE; 0x7F; 0xF7; 0xFB; 0xBF |]
     ; [| 0xFB; 0xEF; 0xFE; 0xBF; 0xFD; 0x7F; 0xF7; 0xDF |]
     ; [| 0xF7; 0xFE; 0xBF; 0xDF; 0xFB; 0xFD; 0x7F; 0xEF |]
     ; [| 0xEF; 0x7F; 0xFD; 0xFB; 0xDF; 0xBF; 0xFE; 0xF7 |]
     ; [| 0xDF; 0xF7; 0x7F; 0xFD; 0xBF; 0xFE; 0xEF; 0xFB |]
     ; [| 0xBF; 0xFB; 0xF7; 0x7F; 0xFE; 0xEF; 0xDF; 0xFD |]
     ; [| 0x7F; 0xBF; 0xDF; 0xEF; 0xF7; 0xFB; 0xFD; 0xFE |]
    |]
  ;;

  let get_term_elt num =
    match num mod 30 with
    | 0 -> 0xFF
    | n when n >= 29 -> 0xFF
    | n when n >= 23 -> 0x7F
    | n when n >= 19 -> 0x3F
    | n when n >= 17 -> 0x1F
    | n when n >= 13 -> 0x0F
    | n when n >= 11 -> 0x07
    | n when n >= 7 -> 0x03
    | _ -> 0x01
  ;;

  let get_head_elt num =
    match num mod 30 with
    | 0 -> 0x00
    | n when n > 23 -> 0x80
    | n when n > 19 -> 0xC0
    | n when n > 17 -> 0xE0
    | n when n > 13 -> 0xF0
    | n when n > 11 -> 0xF8
    | n when n > 7 -> 0xFC
    | n when n > 1 -> 0xFE
    | _ -> 0xFF
  ;;

  let rem_to_bit r =
    match r with
    | 1 -> 0
    | 7 -> 1
    | 11 -> 2
    | 13 -> 3
    | 17 -> 4
    | 19 -> 5
    | 23 -> 6
    | 29 -> 7
    | _ -> failwith "invalid remainder"
  ;;

  let range start stop = List.init (stop - start) (fun n -> n + start)

  let make_small_tbl upper =
    assert (upper >= 2);
    let tbl_size = (upper + 29) / 30 in
    let tbl = Array.make tbl_size '\xFF' in
    let sieve q =
      let rec flag_loop flag =
        if flag <> 0
        then (
          let r_bit = Util.get_NTZ (flag land -flag) in
          let rec mask_loop idx b_bit =
            if idx < tbl_size
            then (
              tbl.(idx) <- Char.chr (Char.code tbl.(idx) land mask_tbl.(r_bit).(b_bit));
              mask_loop (idx + get_idx_offset q r_bit b_bit) (succ b_bit mod 8))
          in
          let r = mod30.(r_bit) in
          mask_loop ((q * ((30 * q) + (2 * r))) + (r * r / 30)) r_bit;
          flag_loop (flag land (flag - 1)))
      in
      flag_loop (Char.code tbl.(q))
    in
    tbl.(0) <- '\xFE';
    tbl.(tbl_size - 1) <- Char.chr (get_term_elt upper land Char.code tbl.(tbl_size - 1));
    List.iter sieve (range 0 ((isqrt upper + 29) / 30));
    tbl
  ;;

  let make_tbl low high =
    assert (high >= low);
    assert (low >= 7);
    if low = 7
    then make_small_tbl high
    else (
      let o_low = (low - 1) / 30 in
      let o_high = (high - 1) / 30 in
      let tbl_size = o_high - o_low + 1 in
      let tbl = Array.make tbl_size '\xFF' in
      let rec sieve = function
        | [] -> ()
        | (i, bits) :: xs ->
          let rec flag_loop flag =
            if flag <> 0
            then (
              let r_bit = Util.get_NTZ (flag land -flag) in
              let prime = (30 * i) + mod30.(r_bit) in
              let tmp =
                idx_to_num (num_to_idx (max ((low + prime - 1) / prime) prime)) * prime
              in
              let rec mask_loop idx b_bit =
                if idx - o_low < tbl_size
                then (
                  tbl.(idx - o_low)
                  <- Char.chr (Char.code tbl.(idx - o_low) land mask_tbl.(r_bit).(b_bit));
                  mask_loop (idx + get_idx_offset i r_bit b_bit) (succ b_bit mod 8))
              in
              mask_loop (tmp / 30) (rem_to_bit (tmp / prime mod 30));
              flag_loop (flag land (flag - 1)))
          in
          flag_loop bits;
          sieve xs
      in
      tbl.(0) <- Char.chr (get_head_elt low);
      tbl.(tbl_size - 1) <- Char.chr (get_term_elt high land Char.code tbl.(tbl_size - 1));
      sieve
        (make_small_tbl (isqrt high)
         |> Array.to_list
         |> List.mapi (fun idx flags -> (idx, Char.code flags)));
      tbl)
  ;;

  let primes low high =
    assert (high >= low);
    let lst = ref [] in

    if low <= 2 && 2 <= high then lst := 2 :: !lst;
    if low <= 3 && 3 <= high then lst := 3 :: !lst;
    if low <= 5 && 5 <= high then lst := 5 :: !lst;
    if high < 7
    then List.rev !lst
    else (
      let low = max low 7 in
      let offset = low / 30 in

      let aux (q, lst) bits =
        let rec flag_loop flag res =
          if flag = 0
          then (succ q, res)
          else
            flag_loop
              (flag land (flag - 1))
              (((30 * q) + mod30.(Util.get_NTZ (flag land -flag))) :: res)
        in
        flag_loop (Char.code bits) lst
      in
      let _, res = Array.fold_left aux (offset, !lst) (make_tbl low high) in
      List.rev res)
  ;;
end

(* Divisor function *)
(* https://en.wikipedia.org/wiki/Divisor_function *)
let get_sigma_tbl z upper =
  let p_lst = Prime.primes 1 upper in
  let tbl = Array.init (upper + 1) (fun _ -> 1) in
  let f1 p =
    let rec aux q x =
      if q <= upper
      then (
        let next_x = x + int_pow q z in
        tbl.(q) <- tbl.(q) + next_x;
        aux (q * p) next_x)
    in
    aux p 0
  in
  let f2 p =
    let rec aux q =
      if q <= upper
      then (
        for n = 2 to upper / q do
          if tbl.(n) <> 1 && n mod p <> 0 then tbl.(q * n) <- tbl.(q) * tbl.(n)
        done;
        aux (q * p))
    in
    aux p
  in
  List.iter f1 p_lst;
  List.iter f2 p_lst;
  tbl.(0) <- 0;
  tbl
;;
