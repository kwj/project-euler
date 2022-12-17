(* Project Euler: Problem 36 *)

open Core

let solve num =
  let rec aux n result =
    if n < 1 then
      result
    else
      if n <> Int.(of_string (String.rev (to_string n))) then
        aux (pred n) result
      else
        let tmp = Euler.Math.bin_of_int n in
        if String.(tmp = String.rev tmp) then
          aux (pred n) (result + n)
        else
          aux (pred n) result
  in
  aux num 0

let exec () =
  Int.to_string (solve 1_000_000)

let () = Euler.Task.run exec
