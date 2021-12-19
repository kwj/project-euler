(* Project Euler: Problem 82 *)

(*
  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p082_matrix.txt
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
  |> List.map (fun l -> Array.of_list (List.map (int_of_string) l))

let transpose arr_lst =
  let x = Array.length (List.hd arr_lst) in
  let y = List.length arr_lst in
  let src_matrix = Array.of_seq (List.to_seq arr_lst) in
  let m_arr = Array.make_matrix x y 0 in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      m_arr.(i).(j) <- src_matrix.(j).(i)
    done
  done;
  List.of_seq @@ Array.to_seq m_arr

let find_min_path arr_lst =
  let rec loop work_arr lst =
    match lst with
    | [] ->
       let min_val = ref max_int in
       for i = 0 to Array.length work_arr - 1 do
         min_val := min !min_val work_arr.(i)
       done;
       !min_val
    | arr :: tl ->
       let len = Array.length work_arr in
       work_arr.(0) <- work_arr.(0) + arr.(0);
       for i = 1 to len - 1 do
         work_arr.(i) <- min (work_arr.(i) + arr.(i)) (work_arr.(i - 1) + arr.(i))
       done;
       for i = len - 2 downto 0 do
         work_arr.(i) <- min work_arr.(i) (work_arr.(i + 1) + arr.(i))
       done;
       loop work_arr tl
  in
  loop (List.hd arr_lst) (List.tl arr_lst)

let () =
  Printf.printf "Answer: %d\n" (read_data() |> cnvt_data |> transpose |> List.rev |> find_min_path)
