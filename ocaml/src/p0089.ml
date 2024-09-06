(* Project Euler: Problem 89 *)

(*
 * step 1:
 *   IIIIIIIII     IX
 *   XXXXXXXXX     XC
 *   CCCCCCCCC     CM
 *
 * step 2:
 *   VIIII         IX
 *   LXXXX         XC
 *   DCCCC         CM
 *
 * step 3:
 *   IIIII         V
 *   XXXXX         L
 *   CCCCC         D
 *
 * step 4:
 *   IIII          IV
 *   XXXX          XL
 *   CCCC          CD
 *
 * We will need the following files to run this program.
 *   - https://projecteuler.net/project/resources/p089_roman.txt
 *)

open Core

let replace s =
  Str.global_replace (* step 1: IX/XC/CM *)
    (Str.regexp "IIIIIIIII\\|XXXXXXXXX\\|CCCCCCCCC")
    "##"
    s
  |> Str.global_replace (Str.regexp "VIIII\\|LXXXX\\|DCCCC") "##" (* step 2: IX/XC/CM *)
  |> Str.global_replace (Str.regexp "IIIII\\|XXXXX\\|CCCCC") "#" (* step 3: V/L/D *)
  |> Str.global_replace (Str.regexp "IIII\\|XXXX\\|CCCC") "##" (* step 4: IV/XL/CD *)
;;

let compute str_lst =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (acc + (String.length x - String.length (replace x))) xs
  in
  loop 0 str_lst
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p089_roman.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p089_roman.txt")) 743
;;
