(* Project Euler: Problem 62 *)

open Core

let solve () =
  let make_key num =
    List.sort ~compare:String.compare (Str.split (Str.regexp "") (Int.to_string num))
    |> List.fold ~init:"" ~f:(^)
  in
  let update_tbl tbl key data =
    match Hashtbl.find tbl key with
    | None -> Hashtbl.set tbl ~key ~data:[data]; [data]
    | Some v -> Hashtbl.set tbl ~key ~data:(data :: v); data :: v
  in
  let tbl = Hashtbl.create (module String) in

  let rec loop n =
    let cube = n * n * n in
    let cube_lst = update_tbl tbl (make_key cube) cube in
    if List.length cube_lst = 5 then
      List.last_exn cube_lst
    else
      loop (succ n)
  in
  loop 1

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
