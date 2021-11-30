(* Project Euler: Problem 40 *)

(*
    0.123456789 | 10111213...979899 | 100101102...99799899 | 100010011002  ...
      ---------   -----------------   --------------------   ----------------
  len: 1 * 9       2 * 90              3 * 900                4 * 9000     ...
       1 * 9 * 1   2 * 9 * 10          3 * 9 * 100            4 * 9 * 1000 ...

  block #1: 1-digit number
  block #2: 2-digits number
  block #3: 3-digits number
    ...
  block #n: n-digits number

  sum_{i=i}^{n} digits = sum_{i=i}^{n} i * 9 * 10^(i-1)
 *)

(* ---------------------------------------------------------------- *)

(*
let mk_digit_blocks limit =
  let rec aux cnt digits result =
    if digits >= limit then
      result
    else
      let x = int_of_float (float_of_int 10 ** float_of_int (cnt - 1)) in
      let tmp = cnt * 9 * x in
      aux (succ cnt) (tmp + digits) ((digits, tmp + digits, cnt, x) :: result)
  in
  List.rev (aux 1 0 [])
 *)

let identify_range i =
  let digit_blocks =
    [(0, 9, 1, 1); (9, 189, 2, 10); (189, 2889, 3, 100); (2889, 38889, 4, 1000);
     (38889, 488889, 5, 10000); (488889, 5888889, 6, 100000)] in   (* mk_digit_blocks 1000000 *)
  let rec aux lst =
    match lst with
    | ((a1, a2, _, _) as hd) :: tl when a1 < i && i <= a2 -> hd
    | hd :: tl -> aux tl
    | [] -> failwith "out of range"
  in
  aux digit_blocks

let d i =
  let bottom, _, width, base = identify_range i in
  let num = base + int_of_float (float_of_int (i - 1 - bottom) /. float_of_int width) in
  let idx = (i - 1 - bottom) mod width in
  int_of_char (String.get (string_of_int num) idx) - int_of_char '0'

let solve () =
  (d 1) * (d 10) * (d 100) * (d 1000) * (d 10000) * (d 100000) * (d 1000000)

let () =
  Printf.printf "Answer: %d\n" (solve ())
