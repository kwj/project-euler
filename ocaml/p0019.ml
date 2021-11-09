(* Project Euler: Problem 19 *)

(*
  dow: day of week  [0: Sunday, 1: Monday, 2: Tuesday, ..., 6: Saturday]
 *)

let count_sundays start_year end_year dow_of_Jan1 =
  let days_in_months y =
    if (y mod 400 = 0) || (y mod 4 = 0 && y mod 100 <> 0) then
      [31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]    (* leap year *)
    else
      [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]    (* common year *)
  in
  let next_dow a b = (a + b) mod 7, (a + b) mod 7 in
  let dow = ref dow_of_Jan1 in
  let cnt = ref 0 in
  for i = start_year to end_year do
    let (t_dow, lst) = List.fold_left_map next_dow !dow (days_in_months i) in
      dow := t_dow;
      cnt := !cnt + (List.length @@ List.filter (fun n -> n = 0) lst)
  done;
  if !dow = 0 then
    !cnt - 1
  else
    !cnt

let () =
  Printf.printf "number of Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000) is %d\n"
    ((count_sundays 1900 2000 1) - (count_sundays 1900 1900 1))
