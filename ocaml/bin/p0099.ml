(* Project Euler: Problem 99 *)

(*
  log10 base^exp = exp * (log10 base)

  We will need the following file to run this program.
    - https://projecteuler.net/project/resources/p099_base_exp.txt
 *)

open Core

let parse_data data =
  List.map ~f:(fun l -> Str.split (Str.regexp ",") l |> List.map ~f:Float.of_string) data
  |> List.mapi ~f:(fun i be_lst -> (List.nth_exn be_lst 1) *. (Float.log10 (List.nth_exn be_lst 0)), (i + 1))

let solve lst =
  List.sort ~compare:(fun (f1, _) (f2, _) -> Float.compare f2 f1) lst
  |> List.hd_exn
  |> snd

let exec data =
  Int.to_string (solve (parse_data data))

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
