(* Project Euler: Problem 4 *)

let rev_str str =
  let rec aux idx =
    match idx with
      0 -> Char.escaped str.[idx]
    | _ -> Char.escaped str.[idx] ^ aux (idx - 1)
  in
  aux (String.length str - 1);;

let mk_list =
  let lst = ref [] in
  for x = 100 to 999 do
    for y = x to 999 do
      let tmp = x * y in
      if tmp = int_of_string (rev_str (string_of_int tmp)) then
        lst := tmp :: !lst
    done
  done;
  List.rev (List.sort compare !lst);;

let () =
  Printf.printf "the largest palindrome made from the product of two 3-digit numbers is %d\n" (List.hd mk_list);;
