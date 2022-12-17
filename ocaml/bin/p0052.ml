(* Project Euler: Problem 52 *)

open Core

let check_number num =
  let sorted_digit num =
    Str.split (Str.regexp "") (Int.to_string num)
    |> List.sort ~compare:String.compare
    |> List.fold ~init:"" ~f:(^)
  in
  let id_s = sorted_digit num in
  let rec aux = function
    | x :: xs -> if String.(sorted_digit (num * x) = id_s) then aux xs else false
    | [] -> true
  in
  aux (List.range 2 7)

let rec loop d =
  let ulimit = (Int.pow 10 d) / 6 in
  let rec aux n =
    if n > ulimit then
      None
    else
      match check_number n with
      | false -> aux (succ n)
      | true -> Some n
  in
  match aux (Int.pow 10 (d - 1)) with
  | Some n -> n
  | None -> loop (succ d)

let exec () =
  Int.to_string (loop 1)

let () = Euler.Task.run exec
