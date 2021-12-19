(* Project Euler: Problem 67 *)

(*
  You will need the following files to run this program.
   - https://projecteuler.net/project/resources/p067_triangle.txt
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
  List.map (fun l -> Str.split (Str.regexp " ") l |> List.map int_of_string) data
    
let calc_from_bottom l_lst =
  let add_max_leaf a b =
    let rec select_leaf lst =
      match lst with
      | a :: b :: [] -> [max a b]
      | a :: (b :: _ as tl) -> (max a b) :: (select_leaf tl)
      | _ -> assert false
    in
    List.map2 (+) a (select_leaf b)
  in
  List.hd @@
    List.fold_right add_max_leaf l_lst (List.init ((List.length @@ List.hd @@ List.rev l_lst) + 1) (fun n -> 0))

let () =
  Printf.printf "Answer: %d\n" (read_data() |> cnvt_data |> calc_from_bottom)
