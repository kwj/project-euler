(* Project Euler: Problem 36 *)

(*
  >>> import math
  >>> math.log(10 ** math.ceil(math.log(1000000, 2)), 2)
  66.43856189774725

  # Sys.int_size;;
  - : int = 63

  So, I handle binary numbers as string.
 *)

(* ---------------------------------------------------------------- *)

let rev_str str =
  let str_len = String.length str in
  let result = Buffer.create str_len in
  for i = str_len - 1 downto 0 do
    Buffer.add_char result str.[i]
  done;
  Buffer.contents result

let to_binary_str num =
  let result = Buffer.create 16 in
  let rec aux num =
    if num = 0 then
      rev_str (Buffer.contents result)
    else (
      Buffer.add_char result (char_of_int ((num mod 2) + (int_of_char '0')));
      aux (num / 2)
    )
  in
  aux num

let solve num =
  let rec aux n result =
    if n < 1 then
      result
    else
      if n <> int_of_string (rev_str (string_of_int n)) then
        aux (pred n) result
      else
        let tmp = to_binary_str n in
        if tmp = rev_str tmp then
          aux (pred n) (result + n)
        else
          aux (pred n) result
  in
  aux num 0

let () =
  Printf.printf "Answer: %d\n" (solve 1_000_000)
