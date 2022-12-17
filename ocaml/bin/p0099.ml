(* Project Euler: Problem 99 *)

(*
  You will need the following file to run this program.
   - https://projecteuler.net/project/resources/p099_base_exp.txt
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

let cnvt_data lines =
  List.map (fun l -> Str.split (Str.regexp ",") l |> List.map float_of_string) lines
  |> List.mapi (fun i be_lst -> (((List.nth be_lst 1) *. (Float.log10 (List.nth be_lst 0))), (i + 1)))
       
let solve () =
  let n_lst = List.rev (read_data () |> cnvt_data) in
  let _, line_num = List.hd (List.rev (List.sort compare n_lst)) in
  line_num

let () =
  Printf.printf "Answer: %d\n" (solve ())
