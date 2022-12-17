(* Project Euler: Problem 80 *)

(*
  This program is slow because it uses lists.

  I used a method of extracting square root which I was taught in my childhood.
 *)

open Core

let solve num =
  let gen_dividend num =
    let rec aux n result =
      if n = 0 then
        if List.length result mod 2 <> 0 then
          0 :: result
        else
          result
      else
        aux (n / 10) ((n mod 10) :: result)
    in
    let dividend = ref (aux num []) in
    let next l =
      if List.length !dividend = 0 then
        0 :: 0 :: l
      else
        let elm1, elm2 = List.nth_exn !dividend 0, List.nth_exn !dividend 1 in
        dividend := List.tl_exn (List.tl_exn !dividend);
        elm2 :: elm1 :: l
    in
    next
  in
  let next_step divd q =
    let module U = Euler.Util in
    let rec loop x =
      let tmp = U.mul_nlst (U.add_nlst (U.mul_nlst q 20) [x]) x in
      if U.cmp_nlst tmp divd <= 0 then
        (U.sub_nlst divd tmp), (U.trim_zero_nlst (x :: q))
      else
        loop (pred x)
    in
    loop 9
  in
  let extr_sqroot g_divd divd q =
    let rec loop divd q =
      if List.length q >= 100 then
        q
      else
        let next_divd, next_q = next_step divd q in
        loop (g_divd next_divd) next_q
    in
    loop (g_divd divd) q
  in
  let rec compute n result =
    let sum100 lst =
      let rec aux lst =
        if List.length lst > 100 then
          aux (List.tl_exn lst)
        else
          List.fold ~f:(+) ~init:0 lst
      in
      aux lst
    in
    if n > num then
      result
    else
      let sqrt_n = Euler.Math.isqrt n in
      if n = sqrt_n * sqrt_n then
        compute (n + 1) result
      else
        compute (n + 1) (result + (sum100 (extr_sqroot (gen_dividend n) [] [0])))
  in
  compute 1 0

let exec () =
  Int.to_string (solve 100)

let () = Euler.Task.run exec
