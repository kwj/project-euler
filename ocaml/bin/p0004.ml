(* Project Euler: Problem 4 *)

open Core

let exec () =
  let lst = ref [] in
  for x = 100 to 999 do
    for y = x to 999 do
      let tmp = x * y in
      if tmp = Int.of_string (String.rev (Int.to_string tmp)) then
        lst := tmp :: !lst
    done
  done;
  List.rev (List.sort ~compare !lst) |> List.hd_exn |> Int.to_string

let () = Euler.Task.run exec
