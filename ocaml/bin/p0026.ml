(* Project Euler: Problem 26 *)

open Core

let find_longest_cycle num =
  let div_loop a b =
    let rec aux dividend divisor cnt rems =
      if dividend = 0 then (
        0
      ) else (
        if Bool.equal (List.Assoc.mem rems dividend ~equal) true then
          cnt - (List.Assoc.find_exn rems dividend ~equal)
        else
          aux ((dividend * 10) mod divisor) divisor (succ cnt) ((dividend, cnt) :: rems)
      )
    in
    aux a b 0 []
  in
  let rec aux n max_num max_cycle =
    if n < 1 || n <= max_cycle then
      max_num, max_cycle
    else
      let len = div_loop 1 n in
      if len > max_cycle then
        aux (pred n) n len
      else
        aux (pred n) max_num max_cycle
  in
  aux num 0 0

let exec () =
  let num, cycles = find_longest_cycle 999 in
  sprintf "%d (cycles: %d)" num cycles

let () = Euler.Task.run exec
