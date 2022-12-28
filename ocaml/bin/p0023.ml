(* Project Euler: Problem 23 *)

open Core

let const_upper = 28123

let find_abundant_numbers upper =
  let p_divisors num = List.drop_last_exn (Euler.Math.divisors num) in
  let rec aux num result =
    if num < 12 then  (* 12: minimum abundant number *)
      result
    else
      if List.fold (p_divisors num) ~f:(+) ~init:0 > num then
        aux (pred num) (num :: result)
      else
        aux (pred num) result
  in
  aux upper []

let sum_of_targets an_lst =
  let tgt_table = Array.create ~len:(const_upper + 1) false in
  let an_str = Array.of_list an_lst in
  for i = 0 to (Array.length an_str) - 1 do
    for j = i to (Array.length an_str) - 1 do
      let idx = an_str.(i) + an_str.(j) in
      if idx <= const_upper then
        tgt_table.(idx) <- true
    done
  done;
  Array.foldi tgt_table ~init:0
    ~f:(fun i acc elt -> if Bool.(elt = false) then acc + i else acc)

let exec () =
  find_abundant_numbers const_upper
  |> sum_of_targets
  |> Int.to_string

let () = Euler.Task.run exec
