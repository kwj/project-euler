(* Project Euler: Problem 62 *)

open Core

let compute qty =
  let make_key n = Euler.Util.digits n |> List.sort ~compare |> Euler.Util.undigits in
  let tbl = Hashtbl.create (module Int) in
  let rec loop n =
    let cube = Int.pow n 3 in
    let cube_lst =
      Hashtbl.update_and_return tbl (make_key cube) ~f:(fun v ->
        match v with
        | None -> [ cube ]
        | Some lst -> cube :: lst)
    in
    if List.length cube_lst = qty then List.last_exn cube_lst else loop (succ n)
  in
  loop 1
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "3" = [%test_eq: int] (compute 3) 41063625
let%test_unit "5" = [%test_eq: int] (compute 5) 127035954683
