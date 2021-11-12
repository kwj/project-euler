(* Project Euler: Problem 22 *)

(*
  You will need the following file to run this program.
   - https://projecteuler.net/project/resources/p022_names.txt
 *)

let read_data() =
  let data =
    let filename = ref "" in
    let anon_fun n = () in
    let speclist = [("-f", Arg.Set_string filename, "<filename>  Set input file name (If not specified, read from stdin)")] in
    Arg.parse speclist anon_fun "Usage:";
    if !filename <> "" then (
      let fin = open_in !filename in
      let line = input_line fin in
      close_in fin;
      line
    ) else
      read_line()
  in
  List.sort compare (Str.split (Str.regexp {|","|}) (String.sub data 1 (String.length data - 2)))

let calc_scores lst =
  let value_tbl = Hashtbl.create 26 in    (* 'A'...'Z' *)
  let name_score idx name =
    let name_value s =
      let result = ref 0 in
      String.iter (fun c -> result := !result + Hashtbl.find value_tbl c) s;
      !result
    in
    (idx + 1) * (name_value name)
  in
  String.iteri (fun i c -> Hashtbl.add value_tbl c (i + 1)) "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  List.mapi name_score lst

let () =
  Printf.printf "the total of all the name scores in the file %d\n" (List.fold_left (+) 0 (calc_scores @@ read_data()))
