(* Project Euler: Problem 59 *)

(*
  ASCII
    reference: https://en.wikipedia.org/wiki/ASCII

    lower case characters:
      'a'..'z': 0x61(97d)..0x7a(122d)
    printable characters:
      0x20(32d)..0x7E(126d)
    common English words:
      assume that [A-Za-z]+

  Approach
    step 1:
      try to decrypt with all keys. (brute force)
      if the decrypted data only contains printable characters, the key is a candidate key.
    step 2:
        the key which has the best score by evaluating decrypted data is used as secret key.
        print decrypted text and the sum of ASCII values with secret key.

  You will need the following files to run this program.
   - https://projecteuler.net/project/resources/p059_cipher.txt   [encrypted data]
 *)

(* ---------------------------------------------------------------- *)

let read_data() =
  let filename = ref "" in
  let anon_fun n = () in
  let speclist = [("-f", Arg.Set_string filename, "<filename>  Set ecrypted file name (If not specified, read from stdin)")] in
  Arg.parse speclist anon_fun "Usage:";
  let enc_data =
    if !filename <> "" then (
      let fin = open_in !filename in
      let rec loop acc =
        match input_line fin with
        | l -> loop (l :: acc)
        | exception End_of_file -> close_in fin; List.rev acc
      in
      loop []
    ) else (
      let rec loop acc =
        match read_line () with
        | l -> loop (l :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop []
    )
  in
  List.map int_of_string (List.flatten (List.map (fun line -> Str.split (Str.regexp ",") line) enc_data))

let decrypt enc_lst key_lst =
  let rec aux lst result =
    match lst with
    | e1 :: e2 :: e3 :: tl ->
       aux tl ((e3 lxor (List.nth key_lst 2)) :: (e2 lxor (List.nth key_lst 1)) :: (e1 lxor (List.nth key_lst 0)) :: result)
    | e1 :: e2 :: [] ->
       aux [] ((e2 lxor (List.nth key_lst 1)) :: (e1 lxor (List.nth key_lst 0)) :: result)
    | e1 :: [] ->
       aux [] ((e1 lxor (List.nth key_lst 0)) :: result)
    | _ ->
       List.rev result
  in
  aux enc_lst []

let step1 enc_lst =
  let is_plaintext lst =
    let is_non_printable c = if c < 32 || c > 126 then true else false in
    if List.exists is_non_printable lst then false else true
  in
  let cands_tbl = Hashtbl.create 128 in      (* 128 is a tentative value, meaningless *)
  (* 'a'=97, 'z'=122  *)
  for k1 = 97 to 122 do
    for k2 = 97 to 122 do
      for k3 = 97 to 122 do
        let dec_lst = decrypt enc_lst [k1; k2; k3] in
        if is_plaintext dec_lst then
          Hashtbl.add cands_tbl [k1; k2; k3] dec_lst
      done
    done
  done;
  cands_tbl

let step2 cands_tbl =
  let lstint_to_str lst =
    let b = Buffer.create 1500 in
    List.iter (Buffer.add_char b) (List.map char_of_int lst);
    Buffer.contents b
  in
  let eval_str str =
    (* terrible strategy ... *)
    let lst = Str.split (Str.regexp "[^A-Za-z]+") str in
    (List.length (List.filter ((=) "the") lst))
    + (List.length (List.filter ((=) "of") lst))
    + (List.length (List.filter ((=) "is") lst))
    + (List.length (List.filter ((=) "this") lst))
  in
  let _, k =
    Seq.map (fun (k, ilst) -> (eval_str (lstint_to_str ilst)), k) (Hashtbl.to_seq cands_tbl)
    |> List.of_seq
    |> List.sort (fun (n1, k1) (n2, k2) -> n2 - n1)
    |> List.hd
  in
  Printf.printf "%s\n" (lstint_to_str (Hashtbl.find cands_tbl k));
  Printf.printf "sum=%d, key=%s\n" (List.fold_left (+) 0 (Hashtbl.find cands_tbl k)) (lstint_to_str k)

let () =
  let enc_data = read_data () in
  let cands_tbl = step1 enc_data in
  step2 cands_tbl

