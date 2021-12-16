(* Project Euler: Problem 19 *)

(*
  Jan 1, 1900 was Monday.
  -->
    day of week  [0: Sunday, 1: Monday, 2: Tuesday, ..., 6: Saturday]

  Jan 1, 1901 was Tuesday since (1 + 365) mod 7 = 2
 *)

let count_sundays () =
  let cu_sum lst =
    let rec aux pre acc = function
      | [] -> List.rev acc
      | hd :: tl -> aux (pre + hd) ((pre + hd) :: acc) tl
    in
    aux 0 [] lst
  in
  let common_year = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  let leap_year = [31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  let year_2000 = List.rev(List.tl (List.rev common_year)) in
  let rec loop i acc =
    if i = 0 then
      List.flatten acc
    else
      if i mod 4 = 0 then
        loop (pred i) (leap_year :: acc)
      else
        loop (pred i) (common_year :: acc)
  in
  let dpm = loop 99 (year_2000 :: []) in    (* days per month: Jan 1901 - Nov 2000 *)

  List.length (List.filter (fun n -> n mod 7 = 0) (cu_sum ((1 + (List.fold_left (+) 0 common_year) mod 7) :: dpm)))

let () =
  Printf.printf "Answer: %d\n" (count_sundays())
