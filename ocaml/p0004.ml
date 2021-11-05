(* Project Euler: Problem 4 *)

let rev_str str =
  let str_len = String.length str in
  let result = Buffer.create str_len in
  for i = str_len - 1 downto 0 do
    Buffer.add_char result str.[i]
  done;
  Buffer.contents result

let mk_list =
  let lst = ref [] in
  for x = 100 to 999 do
    for y = x to 999 do
      let tmp = x * y in
      if tmp = int_of_string (rev_str (string_of_int tmp)) then
        lst := tmp :: !lst
    done
  done;
  List.rev (List.sort compare !lst)

let () =
  Printf.printf "the largest palindrome made from the product of two 3-digit numbers is %d\n" (List.hd mk_list)
