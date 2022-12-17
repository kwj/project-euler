(* Project Euler: Problem 89 *)

(*
  step 1:
    IIIIIIIII     IX
    XXXXXXXXX     XC
    CCCCCCCCC     CM

  step 2:
    VIIII         IX
    LXXXX         XC
    DCCCC         CM

  step 3:
    IIIII         V
    XXXXX         L
    CCCCC         D

  step 4:
    IIII          IV
    XXXX          XL
    CCCC          CD

  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p089_roman.txt
 *)

open Core

let replace s =
  Str.global_replace (Str.regexp "IIIIIIIII\\|XXXXXXXXX\\|CCCCCCCCC") "##" s    (* step 1: IX/XC/CM *)
  |> Str.global_replace (Str.regexp "VIIII\\|LXXXX\\|DCCCC") "##"      (* step 2: IX/XC/CM *)
  |> Str.global_replace (Str.regexp "IIIII\\|XXXXX\\|CCCCC") "#"      (* step 3: V/L/D *)
  |> Str.global_replace (Str.regexp "IIII\\|XXXX\\|CCCC") "##"      (* step 4: IV/XL/CD *)

let solve data =
  let rec loop acc = function
      [] -> acc
    | x :: xs -> loop (acc + ((String.length x) - (String.length (replace x)))) xs
  in
  loop 0 data

let exec data =
  Int.to_string (solve data)

let () = Euler.Task.run_with_data exec (Euler.Task.read_data ())
