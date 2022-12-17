(* Project Euler: Problem 98 *)

(*
  You will need the following file to run this program.
   - https://projecteuler.net/project/resources/p098_words.txt
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
  let data = List.hd lines in
  let word_wkey = Str.split (Str.regexp {|","|}) (String.sub data 1 (String.length data - 2))
                  |> List.map (fun s -> (List.fold_left (^) "" (List.sort compare (Str.split (Str.regexp "") s)), s))
                  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) in
  let rec loop lst acc result =
    match lst with
    | [] -> List.rev result
    | (k1, w1) :: ((k2, w2) :: _ as tl) when k1 = k2 -> loop tl (w1 :: acc) result
    | (k1, w1) :: tl -> loop tl [] ((k1, (w1 :: acc)) :: result)
  in
  loop word_wkey [] [] |> List.filter (fun (k, wl) -> List.length wl > 1)

let rec uniq = function
  | [] -> []
  | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> uniq tl
  | hd :: tl -> hd :: uniq tl

let make_squares wlst =
  let add_record len tbl =
    let pow b e =
      let rec loop n acc =
        match n with
        | n when n < 1 -> acc
        | _ -> loop (pred n) (acc * b)
      in
      loop e 1
    in
    let low = pow 10 (len - 1) in
    let rec aux n limit acc =
      let sq = n * n in
      if sq >= limit then
        List.rev @@ List.map string_of_int @@ List.filter (fun i -> i >= low) acc
      else
        aux (succ n) limit (sq :: acc)
    in
    Hashtbl.replace tbl len (aux (truncate @@ sqrt @@ float low) (pow 10 len) []);
    tbl
  in      
  let keys, _ = List.split wlst in
  let len_lst = List.map (fun k -> String.length k) keys |> List.sort compare |> uniq in
  let rec loop lst tbl =
    match lst with
    | [] -> tbl
    | hd :: tl -> loop tl (add_record hd tbl)
  in
  loop len_lst (Hashtbl.create (List.hd (List.rev len_lst)))
  
let is_anagramic_sq w_lst sq_lst =
  let is_mismatch map_lst =
    let rec loop = function
      | [] -> false
      | (k1, v1) :: (k2, v2) :: _ when k1 = k2 && v1 <> v2 -> true
      | (_, _) :: tl -> loop tl
    in
    loop (List.sort compare map_lst) || loop (List.sort compare (List.map (fun (k, v) -> (v, k)) map_lst))
  in
  let rec check_sq word lst acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
       let kv_map = List.combine (Str.split (Str.regexp "") word) (Str.split (Str.regexp "") hd) in
       if is_mismatch kv_map then
         check_sq word tl acc
       else
         check_sq word tl (kv_map :: acc)
  in
  let cnvt_word word kv_lst =
    List.map (fun ch_str -> List.assoc ch_str kv_lst) (Str.split (Str.regexp "") word)
    |> List.fold_left (^) ""
  in
  let rec loop_kvmap lst acc =
    match lst with
    | [] ->
       if acc = [] then None
       else
         Some (List.hd (List.rev (List.sort compare (List.flatten acc))))
    | kv_map :: tl ->
       let nums_lst = List.map (fun word -> int_of_string (cnvt_word word kv_map)) w_lst in
       if List.mem false (List.map (fun n -> (List.mem (string_of_int n) sq_lst) && Float.(is_integer (sqrt (of_int n)))) nums_lst) then
         loop_kvmap tl acc
       else
         loop_kvmap tl (nums_lst :: acc)

  in
  let kv_map_lst = check_sq (List.hd w_lst) sq_lst [] in
  loop_kvmap kv_map_lst []
       
let solve () =
  let words = List.rev (read_data () |> cnvt_data) in
  let sq_tbl = make_squares words in
  let rec loop acc = function
    | [] -> acc
    | (k, wl) :: tl ->
       match is_anagramic_sq wl (Hashtbl.find sq_tbl (String.length k)) with
       | None -> loop acc tl
       | Some v ->
          if v > acc then
            loop v tl
          else
            loop acc tl
  in
  loop 0 words

let () =
  Printf.printf "Answer: %d\n" (solve ())
