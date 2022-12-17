(* Project Euler: Problem 56 *)

open Core

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let solve_1 () =
  let sum_digits num  =
    let rec aux n acc cnt =
      if Z.(equal n ~$0) then (Z.to_int acc, cnt) else aux Z.(n / ~$10) Z.((n mod ~$10) + acc) (succ cnt)
    in
    aux num Z.zero 0
  in
  let rec loop_base base result =
    if Z.(equal base zero) then
      result
    else
      let rec loop_exp power crnt_max =
        if Z.(leq power ~$1) then
          crnt_max
        else
          let digital_sum, num_of_digits = sum_digits power in
          if (num_of_digits * 9) < crnt_max then
            loop_exp Z.zero crnt_max    (* no need to look further *)
          else
            loop_exp Z.(power / base) Int.(max digital_sum crnt_max)
      in
      loop_base Z.(pred base) (loop_exp Z.(base ** 99) result)
  in
  loop_base (Z.of_int 99) 0
      
(* -- method 2 -- *)
(* using list instead of arbitrary-precision arithmetic library *)
let solve_2 () =
  let module U = Euler.Util in
  let sum_digits nlst = List.fold ~f:(+) ~init:0 nlst in
  let rec loop_base base result =
    if base = 1 then
      result
    else
      let rec loop_exp lst exp crnt_max =
        if exp >= 100 then
          crnt_max
        else
          let tmp = U.mul_nlst lst base in
          loop_exp tmp (succ exp) (Int.max (sum_digits tmp) crnt_max)
      in
      loop_base (pred base) (loop_exp (U.nlst_of_int base) 2 result)
  in
  loop_base 99 0

let exec () =
  sprintf "%d: (w/ Zarith module)\n%d: (w/o Zarith module)" (solve_1 ()) (solve_2 ())

let () = Euler.Task.run exec
