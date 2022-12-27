(* Project Euler: Problem 40 *)

(*
    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
      ---------   -----------------   ---------------------   -----------------
  len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
       1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
         --> block_num * 9 * base

  block #1: 1-digit number
  block #2: 2-digits number
  block #3: 3-digits number
    ...
  block #n: n-digits number

  sum_{i=i}^{n} digits = sum_{i=i}^{n} i * 9 * 10^(i-1)
 *)

open Core

(*
let mk_digit_blocks limit =
  let rec aux block_num total_size result =
    if total_size >= limit then
      result
    else
      let base = Int.pow 10 (block_num - 1) in
      let block_size = block_num * 9 * base in
      aux (succ block_num) (total_size + block_size) ((total_size + 1, total_size + 1 + block_size, block_num, base) :: result)
  in
  List.rev (aux 1 0 [])
 *)

let select_block nth =
  (* mk_digit_blocks 1_000_000 *)
  let digit_blocks = [(1, 10, 1, 1); (10, 190, 2, 10); (190, 2890, 3, 100); (2890, 38890, 4, 1000);
                      (38890, 488890, 5, 10000); (488890, 5888890, 6, 100000)] in
  let rec aux lst =
    match lst with
    | ((a1, a2, _, _) as x) :: _ when a1 <= nth && nth < a2 -> x
    | _ :: xs -> aux xs
    | [] -> failwith "out of range"
  in
  aux digit_blocks

let d nth =
  (*
    0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
                    ^[d12]=1
    example (when d12)
      number = 10 + to_int(12.0 - 10. /. 2.) = 11
      offset = (12 - 10) mod 2 = 0

      number: 11
      offset: 01
              ^
   *)
  let bottom, _, width, base = select_block nth in
  let number = base + Int.of_float (Float.(of_int Int.(nth - bottom) / (of_int width))) in
  let offset = (nth - bottom) mod width in
  Char.to_int (String.get (Int.to_string number) offset) - Char.to_int '0'

let solve () =
  (d 1) * (d 10) * (d 100) * (d 1_000) * (d 10_000) * (d 100_000) * (d 1_000_000)

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
