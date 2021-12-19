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

  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p089_roman.txt
 *)

(* ---------------------------------------------------------------- *)

let read_data() =
  let filename = ref "" in
  let anon_fun n = () in
  let speclist = [("-f", Arg.Set_string filename, "<filename>  Set input file name (If not specified, read from stdin)")] in
  Arg.parse speclist anon_fun "Usage:";
  if !filename <> "" then
    let fin = open_in !filename in
    let rec loop acc =
      match input_line fin with
      | l -> loop (l :: acc)
      | exception End_of_file -> close_in fin; List.rev acc
    in
    loop []
  else
    let rec loop acc =
      match read_line () with
      | l -> loop (l :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []

let optimize s =
  Str.global_replace (Str.regexp "IIIIIIIII\\|XXXXXXXXX\\|CCCCCCCCC") "##" s    (* step 1: IX/XC/CM *)
  |> Str.global_replace (Str.regexp "VIIII\\|LXXXX\\|DCCCC") "##"      (* step 2: IX/XC/CM *)
  |> Str.global_replace (Str.regexp "IIIII\\|XXXXX\\|CCCCC") "#"      (* step 3: V/L/D *)
  |> Str.global_replace (Str.regexp "IIII\\|XXXX\\|CCCC") "##"      (* step 4: IV/XL/CD *)

let solve () =
  let rec loop diff lst =
    match lst with
    | [] -> diff
    | hd :: tl -> loop (diff + ((String.length hd) - (String.length (optimize hd)))) tl
  in
  loop 0 (read_data())

let () =
  Printf.printf "Answer: %d\n" (solve ())

