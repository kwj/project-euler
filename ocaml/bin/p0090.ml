(* Project Euler: Problem 90 *)

open Core

let product_lst xs ys =
  List.fold_right ~init:[] ~f:(fun x acc -> (List.map ~f:(fun y -> (x, y)) ys) :: acc) xs
  |> List.concat

let make_numbers d1 d2 =
  product_lst d1 d2
  |> List.map ~f:(fun (f1, f2) -> [f1 ^ f2; f2 ^ f1])
  |> List.concat

let proc_69 lst =
  if List.exists lst ~f:(String.equal "6") || List.exists lst ~f:(String.equal "9") then
    List.dedup_and_sort (["6"; "9"] @ lst) ~compare:String.compare
  else
    lst

let solve () =
  let square_lst = ["01"; "04"; "09"; "16"; "25"; "36"; "49"; "64"; "81"] in
  let faces = Euler.Util.combination 6 ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
              |> List.map ~f:proc_69
  in
  let rec loop lst count =
    match lst with
      [] -> count / 2
    | (d1, d2) :: xs -> let num_lst = make_numbers d1 d2 in
                        if List.for_all square_lst ~f:(List.mem num_lst ~equal:String.equal) then
                          loop xs (succ count)
                        else
                          loop xs count
  in
  loop (product_lst faces faces) 0

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
