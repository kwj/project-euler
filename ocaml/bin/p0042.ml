(* Project Euler: Problem 42 *)

(*
  You will need the following file to run this program.
   - https://projecteuler.net/project/resources/p042_words.txt
 *)

(* ---------------------------------------------------------------- *)

module IntSet = Set.Make(Int)

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
  List.sort (fun s1 s2 -> String.length s2 - String.length s1)
    (Str.split (Str.regexp {|","|}) (String.sub data 1 (String.length data - 2)))

let mk_tnumber_set maxlen =
  let limit = maxlen * 26 in
  let f n = n * (n + 1) / 2 in
  let rec aux cnt result =
    let tmp = f cnt in
    if tmp > limit then
      result
    else
      aux (succ cnt) (tmp :: result)
  in
  IntSet.of_list (aux 1 [])

let count_twords wlst =
  let tnum_set = mk_tnumber_set (String.length (List.hd wlst)) in
  let value_tbl = Hashtbl.create 26 in    (* 'A'...'Z' *)
  let name_value s =
    let result = ref 0 in
    String.iter (fun c -> result := !result + Hashtbl.find value_tbl c) s;
    !result
  in
  let rec aux lst result =
    match lst with
    | hd :: tl -> if IntSet.mem (name_value hd) tnum_set then
                    aux tl (succ result)
                  else
                    aux tl result
    | [] -> result
  in
  String.iteri (fun i c -> Hashtbl.add value_tbl c (i + 1)) "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  aux wlst 0

let () =
  Printf.printf "Answer: %d\n" (count_twords @@ read_data())
