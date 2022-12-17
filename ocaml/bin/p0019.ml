(* Project Euler: Problem 19 *)

open Core

let count_sundays () =
  let common_year = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  let leap_year = [31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  let repeat_lst lst n =
    let rec loop i acc =
      if i = 0 then
        acc
      else
        loop (pred i) (List.append acc lst)
    in
    loop n []
  in
  (* days per month (Jan 1901 - Nov 2000) *)
  let days = List.drop_last_exn (repeat_lst (List.append (repeat_lst common_year 3) leap_year) 25) in

  (*
    # day of week - [0: Sunday, 1: Monday, 2: Tuesday, ..., 6: Saturday]
    # Jan 1, 1900 was Monday and assume this day is the first day. (Monday is '1 mod 7 = 1')
    # And then, the year 1900 was common year (365 days).
    # --> Jan 1, 1901 was Tuesday since (1 + 365) mod 7 = 2.
    #     Feb 1, 1901 was Firday since ((1 + 365) + 31) mod 7 = 5.
    #     ... and so on
   *)
  List.folding_map ((1 + 365) :: days) ~init:0 ~f:(fun x y -> x + y, x + y)   (* cumulative sum *)
  |> List.filter ~f:(fun n -> n mod 7 = 0) 
  |> List.length

let exec () =
  Int.to_string (count_sundays ())

let () = Euler.Task.run exec
