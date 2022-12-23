
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

type t = {
    size: int;
    data: char array
  }

let mod30 = [|1; 7; 11; 13; 17; 19; 23; 29|]

(* [*3] *)
let rb_tbl = [|[|0; 0; 0; 0; 0; 0; 0; 1|]; [|1; 1; 1; 0; 1; 1; 1; 1|];
               [|2; 2; 0; 2; 0; 2; 2; 1|]; [|3; 1; 1; 2; 1; 1; 3; 1|];
               [|3; 3; 1; 2; 1; 3; 3; 1|]; [|4; 2; 2; 2; 2; 2; 4; 1|];
               [|5; 3; 1; 4; 1; 3; 5; 1|]; [|6; 4; 2; 4; 2; 4; 6; 1|]|]

(* [*4] *)
let mod30_diff = [|6; 4; 2; 4; 2; 4; 6; 2|]

let get_idx_offset q r b =
  q * mod30_diff.(b) + rb_tbl.(r).(b)

(* [*2] *)
let mask_tbl = [|[|0xFE; 0xFD; 0xFB; 0xF7; 0xEF; 0xDF; 0xBF; 0x7F|];
                 [|0xFD; 0xDF; 0xEF; 0xFE; 0x7F; 0xF7; 0xFB; 0xBF|];
                 [|0xFB; 0xEF; 0xFE; 0xBF; 0xFD; 0x7F; 0xF7; 0xDF|];
                 [|0xF7; 0xFE; 0xBF; 0xDF; 0xFB; 0xFD; 0x7F; 0xEF|];
                 [|0xEF; 0x7F; 0xFD; 0xFB; 0xDF; 0xBF; 0xFE; 0xF7|];
                 [|0xDF; 0xF7; 0x7F; 0xFD; 0xBF; 0xFE; 0xEF; 0xFB|];
                 [|0xBF; 0xFB; 0xF7; 0x7F; 0xFE; 0xEF; 0xDF; 0xFD|];
                 [|0x7F; 0xBF; 0xDF; 0xEF; 0xF7; 0xFB; 0xFD; 0xFE|]|]

let get_term_elt num =
  match num mod 30 with
    0 -> 0xFF
  | n when n >= 29 -> 0xFF
  | n when n >= 23 -> 0x7F
  | n when n >= 19 -> 0x3F
  | n when n >= 17 -> 0x1F
  | n when n >= 13 -> 0x0F
  | n when n >= 11 -> 0x07
  | n when n >= 7 -> 0x03
  | _ -> 0x01

let generate num =
  let tbl_size = (num + 29) / 30 in
  let prime_tbl = Array.make tbl_size '\xFF' in
  let range start stop =
    List.init (stop - start) (fun n -> n + start)
  in
  let sieve_aux q =
    let rec flag_loop flags =
      (*Printf.printf "q=%d flags=%x" q flags; print_endline "";*)
      if flags <> 0 then (
        let r_bit = Util.get_NTZ (flags land (-flags)) in
        let rec mask_loop idx b_bit =
          (*Printf.printf "q=%d idx=%d r_bit=%x b_bit=%x tbl_size=%d" q idx r_bit b_bit tbl_size; print_endline "";*)
          if idx < tbl_size then (
            prime_tbl.(idx) <- Char.chr ((Char.code prime_tbl.(idx)) land (mask_tbl.(r_bit).(b_bit)));
            mask_loop (idx + (get_idx_offset q r_bit b_bit)) ((succ b_bit) mod 8)
          )
        in
        let r = mod30.(r_bit) in
        mask_loop (q * (30 * q + 2 * r) + (r * r) / 30) r_bit;    (* [*1] *)
        flag_loop (flags land (flags - 1))
      )
    in
    flag_loop (Char.code prime_tbl.(q))
  in

  if num < 2 then (
    raise (Invalid_argument "Too small")
  );
  if tbl_size > 1 then (
    prime_tbl.(0) <- '\xFE';  (* remove 1 from the table *)
    prime_tbl.(tbl_size - 1) <- Char.chr (get_term_elt num)
  ) else (
    prime_tbl.(0) <- Char.chr ((get_term_elt num) land 0xFE)
  );
  List.iter sieve_aux (range 0 (((Math.isqrt num) + 29) / 30));
  { size = num; data = prime_tbl }

let get_msb n =
  let x = if n land 0xF0 <> 0 then (n land 0xF0) else n in
  let x = if x land 0xCC <> 0 then (x land 0xCC) else x in
  if x land 0xAA <> 0 then (x land 0xAA) else x

let is_prime p num =
  if num > p.size then
    raise (Invalid_argument "Too large")
  else
    if num = 2 || num = 3 || num = 5 then
      true
    else (
      if num mod 2 = 0 || num mod 3 = 0 || num mod 5 = 0 then
        false
      else
        (Char.code (p.data.(num / 30)) land (get_msb (get_term_elt num))) <> 0
    )
  
let count p =
  match p.size with
    2 -> 1
  | n when n < 5 -> 2
  | n when n < 7 -> 3
  | _ -> Array.fold_left (fun acc elt -> acc + Util.popcount (Char.code elt)) 3 p.data

let to_list p =
  match p.size with
    2 -> [2]
  | n when n < 5 -> [2; 3]
  | n when n < 7 -> [2; 3; 5]
  | _ -> let aux (q, lst) elt =
           let rec flag_loop flags result =
             if flags = 0 then
               ((succ q), result)
             else
               flag_loop (flags land (flags - 1))
                         ((30 * q + mod30.(Util.get_NTZ(flags land (-flags)))) :: result)
           in
           flag_loop (Char.code elt) lst
         in
         let _, lst = Array.fold_left aux (0, [5; 3; 2]) p.data in
         List.rev lst

let to_array p =
  Array.of_list (to_list p)

let prev_prime p num =
  match num with
    n when n > p.size -> raise (Invalid_argument "Too large")
  | n when n <= 2 -> raise (Invalid_argument "Too small")
  | n when n = 3 -> 2
  | n when n <= 5 -> 3
  | n when n <= 7 -> 5
  | n -> let rec aux idx flags =
           if idx < 0 then
             raise Not_found
           else (
             if Char.code p.data.(idx) land flags = 0 then
               aux (pred idx) 0xFF
             else
               30 * idx + mod30.(Util.get_NTZ (get_msb ((Char.code p.data.(idx)) land flags)))
           )
         in
         if n mod 30 <> 1 then
           aux ((n - 1) / 30) (get_term_elt (pred n))
         else
           aux ((n - 2) / 30) 0xFF
