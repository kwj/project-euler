(* Project Euler: Problem 79 *)

(*
  If you look at the log file, you can assume the answer with pen and paper :).

  You will need the following files to run this program.                                                                                              - https://projecteuler.net/project/resources/p079_keylog.txt
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
  List.map (fun l -> Str.split (Str.regexp "") l) data
  |> List.map (fun l -> [(List.nth l 0, List.nth l 1); (List.nth l 0, List.nth l 2); (List.nth l 1, List.nth l 2)])
  |> List.flatten

let assume_passcode tpl_lst =
  let nml_tbl = Hashtbl.create 10 in
  let rev_tbl = Hashtbl.create 10 in
  let rec loop tbl lst =
    match lst with
    | (n1, n2) :: tl ->
       (match Hashtbl.find_opt tbl n1 with
        | None ->
           Hashtbl.add tbl n1 [n2]
        | Some l ->
           if List.mem n2 l then
             ()
           else
             Hashtbl.replace tbl n1 (n2 :: l)
       );
       loop tbl tl
    | _ -> ()
  in
  let tbl_to_str tbl =
    Hashtbl.to_seq tbl
    |> List.of_seq
    |> List.sort (fun (_, l1) (_, l2) -> (List.length l1) - (List.length l2))
    |> List.map (fun (n, l) -> n)
    |> List.fold_left (^) ""
  in
  loop rev_tbl (List.map (fun (x, y) -> (y, x)) tpl_lst);
  loop nml_tbl tpl_lst;
  let tmp = tbl_to_str nml_tbl in
  (String.sub tmp (String.length tmp - 1) 1) ^ (tbl_to_str rev_tbl)

let () =
  Printf.printf "Answer: %s\n" (read_data() |> cnvt_data |> assume_passcode)
