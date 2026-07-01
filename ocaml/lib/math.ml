(* Various functions *)

let rec gcd m n =
  (* Euclidean algorithm *)
  if n = 0 then abs m else gcd n (m mod n)
;;

let lcm m n =
  match (m, n) with
  | 0, _ | _, 0 -> 0
  | m, n -> abs m * (abs n / gcd m n)
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
    if n land 1 = 0 then x * x else x * x * b
;;

(* This function assumes that it runs on 64-bit platforms *)
let _int_powmod b e m =
  let rec aux b e ans =
    if e > 0
    then
      if e land 1 = 1
      then aux (b * b mod m) (e lsr 1) (ans * b mod m)
      else aux (b * b mod m) (e lsr 1) ans
    else ans
  in
  aux (b mod m) e 1
;;

let _z_powmod b e m = Z.(powm (of_int b) (of_int e) (of_int m) |> to_int)

let powmod b e m =
  assert (b >= 0 && m > 0);
  (* 2147483648 = 2^31*)
  if b < 2147483648 && m < 2147483648 then _int_powmod b e m else _z_powmod b e m
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
let isqrt num =
  assert (num >= 0);
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
    let a = aux ((Int.unsigned_bitsize num - 1) / 2) num in
    if num < a * a then a - 1 else a)
;;

let jacobi_symbol a n =
  assert (n > 0 && n land 1 = 1);
  let rec aux sign a n =
    if a = 0
    then if n = 1 then sign else 0
    else if a = 1
    then sign
    else if a land 1 = 0
    then (
      let ntz_a = Int.trailing_zeros a in
      let next_a = a lsr ntz_a in
      let rem = n mod 8 in
      if rem = 1 || rem = 7
      then aux sign next_a n
      else aux (if ntz_a land 1 = 1 then -sign else sign) next_a n)
    else (
      let rem_a = a mod n in
      if rem_a land 1 = 0
      then aux sign rem_a n
      else aux (if rem_a mod 4 = 3 && n mod 4 = 3 then -sign else sign) n rem_a)
  in
  if a < 0 && n mod 4 = 3 then aux (-1) (abs a) n else aux 1 (abs a) n
;;

let _div_loop num b =
  let rec aux n e =
    if n mod b <> 0
    then if e > 0 then Some (n, (b, e)) else None
    else aux (n / b) (succ e)
  in
  aux num 0
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
    let diff = [| 4; 2; 4; 2; 4; 6; 2; 6 |] in
    let rec aux n b idx lst =
      if n / b < b
      then if n <> 1 then (n, 1) :: lst else lst
      else (
        match _div_loop n b with
        | Some (next_n, tpl) ->
          aux next_n (b + diff.(idx)) ((idx + 1) mod Array.length diff) (tpl :: lst)
        | None -> aux n (b + diff.(idx)) ((idx + 1) mod Array.length diff) lst)
    in
    aux n 7 0 res |> List.rev)
;;

let pfactors_to_divisors pf_lst =
  let rec aux divs (base, exp) acc =
    if exp = 0
    then acc
    else aux divs (base, pred exp) (List.map (fun n -> n * int_pow base exp) divs @ acc)
  in
  let rec loop_factor result = function
    | (b, e) :: xs -> loop_factor (aux result (b, e) result) xs
    | [] -> result
  in
  loop_factor [ 1 ] pf_lst |> List.sort compare
;;

let divisors num = factorize num |> pfactors_to_divisors

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

module Prime = struct
  let sprp_bases =
    [| 15591; 2018; 166; 7429; 8064; 16045; 10503; 4399; 1949; 1295; 2776; 3620; 560; 3128; 5212; 2657
     ; 2300; 2021; 4652; 1471; 9336; 4018; 2398; 20462; 10277; 8028; 2213; 6219; 620; 3763; 4852; 5012
     ; 3185; 1333; 6227; 5298; 1074; 2391; 5113; 7061; 803; 1269; 3875; 422; 751; 580; 4729; 10239
     ; 746; 2951; 556; 2206; 3778; 481; 1522; 3476; 481; 2487; 3266; 5633; 488; 3373; 6441; 3344
     ; 17; 15105; 1490; 4154; 2036; 1882; 1813; 467; 3307; 14042; 6371; 658; 1005; 903; 737; 1887
     ; 7447; 1888; 2848; 1784; 7559; 3400; 951; 13969; 4304; 177; 41; 19875; 3110; 13221; 8726; 571
     ; 7043; 6943; 1199; 352; 6435; 165; 1169; 3315; 978; 233; 3003; 2562; 2994; 10587; 10030; 2377
     ; 1902; 5354; 4447; 1555; 263; 27027; 2283; 305; 669; 1912; 601; 6186; 429; 1930; 14873; 1784
     ; 1661; 524; 3577; 236; 2360; 6146; 2850; 55637; 1753; 4178; 8466; 222; 2579; 2743; 2031; 2226
     ; 2276; 374; 2132; 813; 23788; 1610; 4422; 5159; 1725; 3597; 3366; 14336; 579; 165; 1375; 10018
     ; 12616; 9816; 1371; 536; 1867; 10864; 857; 2206; 5788; 434; 8085; 17618; 727; 3639; 1595; 4944
     ; 2129; 2029; 8195; 8344; 6232; 9183; 8126; 1870; 3296; 7455; 8947; 25017; 541; 19115; 368; 566
     ; 5674; 411; 522; 1027; 8215; 2050; 6544; 10049; 614; 774; 2333; 3007; 35201; 4706; 1152; 1785
     ; 1028; 1540; 3743; 493; 4474; 2521; 26845; 8354; 864; 18915; 5465; 2447; 42; 4511; 1660; 166
     ; 1249; 6259; 2553; 304; 272; 7286; 73; 6554; 899; 2816; 5197; 13330; 7054; 2818; 3199; 811
     ; 922; 350; 7514; 4452; 3449; 2663; 4708; 418; 1621; 1171; 3471; 88; 11345; 412; 1559; 194
    |] [@ocamlformat "disable"]
  ;;

  let get_sprp_base n =
    let open Int64 in
    let h1 = of_int n in
    let h2 = mul (logxor (shift_right_logical h1 16) h1) 73244475L in
    let h3 = mul (logxor (shift_right_logical h2 16) h2) 73244475L in
    let idx = to_int (logand (logxor (shift_right_logical h3 16) h3) 255L) in
    sprp_bases.(idx)
  ;;

  let test_sprp n base =
    let s = Int.trailing_zeros (pred n) in
    let d = Int.shift_right_logical (pred n) s in
    let rec aux x =
      if x < 0
      then false
      else if powmod base (d * Int.shift_left 1 x) n = pred n
      then true
      else aux (pred x)
    in
    powmod base d n = 1 || aux (pred s)
  ;;

  let lucas_seq_parameter n =
    if is_square n
    then None
    else
      let open Seq in
      let rec aux seq_d =
        match uncons seq_d with
        | None -> None
        | Some (d, ds) ->
          let k = jacobi_symbol d n in
          if k = -1
          then if d = 5 then Some (5, 5, 5) else Some (d, 1, (1 - d) / 4)
          else if k = 0 && (abs d < n || abs d mod n <> 0)
          then None
          else aux ds
      in
      map2
        (fun a b -> a * b)
        (List.to_seq [ 1; -1 ] |> cycle)
        (unfold (fun x -> Some (x, x + 2)) 5)
      |> aux
  ;;

  let lucas_seq n param_D param_P param_Q =
    let s = Int.trailing_zeros (succ n) in
    let d = Int.shift_right_logical (succ n) s in
    let start_bit = Int.unsigned_bitsize d - 2 in
    let open Z in
    let n_z = of_int n
    and d_z = of_int param_D
    and p_z = of_int param_P
    and q_z = of_int param_Q in
    let rec aux uk vk qk idx =
      if idx < 0
      then (uk, vk, qk, s)
      else (
        let next_uk = uk * uk mod n_z
        and next_vk = ((vk * vk) - (~$2 * qk)) mod n_z
        and next_qk = qk * qk mod n_z in
        if Int.logand (Int.shift_right_logical d idx) 1 = 1
        then (
          let tmp_uk = (p_z * next_uk) + next_vk
          and tmp_vk = (d_z * next_uk) + (p_z * next_vk) in
          aux
            (shift_right (if logand tmp_uk ~$1 = ~$1 then tmp_uk + n_z else tmp_uk) 1
             mod n_z)
            (shift_right (if logand tmp_vk ~$1 = ~$1 then tmp_vk + n_z else tmp_vk) 1
             mod n_z)
            (q_z * next_qk mod n_z)
            (Int.pred idx))
        else aux next_uk next_vk next_qk (Int.pred idx))
    in
    aux ~$1 p_z q_z start_bit
  ;;

  let test_slprp n param_D param_P param_Q =
    let next_v (v : Z.t) (q : Z.t) = Z.(((v * v) - (~$2 * q)) mod of_int n)
    and next_q (q : Z.t) = Z.(q * q mod of_int n)
    and ud, vd, qd, s = lucas_seq n param_D param_P param_Q in
    let vq_pairs =
      Seq.unfold (fun (v, q) -> Some ((v, q), (next_v v q, next_q q))) (vd, qd)
      |> Seq.take s
      |> List.of_seq
    in
    if ud = Z.zero || List.exists (fun (v, _) -> v = Z.zero) vq_pairs
    then (
      let v, q = List.nth vq_pairs (List.length vq_pairs - 1) in
      Some (next_v v q, q))
    else None
  ;;

  let test_vprp v_n1 q n = Z.(v_n1 mod of_int n = of_int q * ~$2 mod of_int n)

  let test_euler_criterion q q_n1_half n =
    Int.popcount (abs q) = 1
    || Z.(q_n1_half mod of_int n |> to_int) = q * jacobi_symbol q n mod n
  ;;

  let strengthened_BPSW_test n =
    test_sprp n 2
    &&
    match lucas_seq_parameter n with
    | None -> false
    | Some (d, p, q) ->
      (match test_slprp n d p q with
       | None -> false
       | Some (v_n1, q_n1_half) ->
         test_vprp v_n1 q n && test_euler_criterion q q_n1_half n)
  ;;

  let is_prime n =
    let small_primes = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 ] in
    if List.mem n small_primes
    then true
    else if List.exists (fun x -> n mod x = 0) small_primes
    then false
    else if n < 1369
    then n > 1
    else if n <= 65535
    then MinFactors.get_minfactor n = 1
    else if n <= 2147483647
    then test_sprp n (get_sprp_base n)
    else strengthened_BPSW_test n
  ;;

  let fermat_prime n = powmod 2 (pred n) n = 1

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
          let r_bit = Int.trailing_zeros (flag land -flag) in
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
              let r_bit = Int.trailing_zeros (flag land -flag) in
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
              (((30 * q) + mod30.(Int.trailing_zeros (flag land -flag))) :: res)
        in
        flag_loop (Char.code bits) lst
      in
      let _, res = Array.fold_left aux (offset, !lst) (make_tbl low high) in
      List.rev res)
  ;;
end

(* Divisor function *)
(* https://en.wikipedia.org/wiki/Divisor_function *)
let sigma_tbl z upper =
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

let aliquot_sum_tbl upper =
  let tbl = sigma_tbl 1 upper in
  Array.mapi_inplace (fun idx v -> v - idx) tbl;
  tbl
;;
