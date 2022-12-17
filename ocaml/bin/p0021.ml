(* Project Euler: Problem 21 *)

open Core

let find_amicable num =
  let p_divisors num = List.drop_last_exn (Euler.Math.divisors num) in
  let rec aux num result =
    if num < 2 then
      result
    else
      if List.mem result num ~equal then
        aux (pred num) result
      else
        let d_num = List.fold ~f:(+) ~init:0 (p_divisors num) in
        let d_x = List.fold ~f:(+) ~init:0 (p_divisors d_num) in
        if num = d_x && d_num <> d_x then
            aux (pred num) (d_num :: (d_x :: result))
        else
          aux (pred num) result
  in
  aux num []

let exec () =
  Int.to_string (List.fold ~f:(+) ~init:0 (find_amicable 10_000))

let () = Euler.Task.run exec
