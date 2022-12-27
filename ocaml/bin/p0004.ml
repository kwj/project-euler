(* Project Euler: Problem 4 *)

open Core

let solve () =
  let lst = ref [] in
  for x = 100 to 999 do
    for y = x to 999 do
      let tmp = x * y in
      if Bool.(Euler.Math.is_palindrome tmp = true) then
        lst := tmp :: !lst
    done
  done;
  List.rev (List.sort ~compare !lst) |> List.hd_exn

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec