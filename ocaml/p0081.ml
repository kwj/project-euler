(* Project Euler: Problem 81 *)

(*
  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p081_matrix.txt
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

let cnvt_data data =
  List.map (fun l -> Str.split (Str.regexp ",") l) data
  |> List.map (fun l -> Array.of_list (max_int :: (List.map (int_of_string) l)))

let find_min_path arr_lst =
  let rec loop work_arr lst =
    match lst with
    | [] ->
       work_arr.((Array.length work_arr) - 1)
    | arr :: tl ->
       for i = 1 to Array.length arr - 1 do
         if arr.(i - 1) < work_arr.(i) then
           arr.(i) <- arr.(i) + arr.(i - 1)
         else
           arr.(i) <- arr.(i) + work_arr.(i)
       done;
       loop arr tl
  in
  let init_arr arr =
    for i = 2 to Array.length arr - 1 do
      arr.(i) <- arr.(i) + arr.(i - 1)
    done;
    arr
  in
  loop (init_arr (List.hd arr_lst)) (List.tl arr_lst)

let () =
  Printf.printf "Answer: %d\n" (read_data() |> cnvt_data |> find_min_path)
