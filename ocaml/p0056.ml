(* Project Euler: Problem 56 *)

(* ---------------------------------------------------------------- *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let solve_1 () =
  let ndigits num = String.length (Z.to_string num) in
  let sum_digits num =
    Z.to_string num
    |> Str.split (Str.regexp "")
    |> List.map int_of_string
    |> List.fold_left (+) 0
  in
  let rec loop_a a result =
    if a = 0 then
      result
    else
      let base = Z.of_int a in
      let rec loop_b b crnt_max =
        if b = 0 then
          crnt_max
        else
          let tmp = Z.pow base b in
          if (ndigits tmp * 9) < crnt_max then
            loop_b 0 crnt_max
          else
            if sum_digits tmp > crnt_max then
              loop_b (pred b) (sum_digits tmp)
            else
              loop_b (pred b) crnt_max
      in
      loop_a (pred a) (loop_b 99 result)
  in
  loop_a 99 0

(* -- method 2 -- *)
(* using list instead of arbitrary-precision arithmetic library *)
let solve_2 () =
  let sum_digits nlst = List.fold_left (+) 0 nlst in
  let lst_of_int num =
    let rec aux n acc =
      if n = 0 then
        List.rev acc
      else
        aux (n / 10) ((n mod 10) :: acc)
    in
    aux num []
  in
  let mul_nlst lst num =
    let rec aux lst carry acc =
      match lst with
      | hd :: tl ->
         aux tl ((hd * num + carry) / 10) (((hd * num + carry) mod 10) :: acc)
      | [] when carry > 0 ->
         aux [] (carry / 10) ((carry mod 10) :: acc)
      | _ ->
         List.rev acc
    in
    aux lst 0 []
  in
  let rec loop_a a result =
    if a = 1 then
      result
    else
      let rec loop_b a_lst b crnt_max =
        if b = 100 then
          crnt_max
        else
          let tmp = mul_nlst a_lst a in
          if sum_digits tmp > crnt_max then
            loop_b tmp (succ b) (sum_digits tmp)
          else
            loop_b tmp (succ b) crnt_max
      in
      loop_a (pred a) (loop_b (lst_of_int a) 2 result)
  in
  loop_a 99 0


let () =
  Printf.printf "Answer:\n";
  Printf.printf "   %d: (w/ Zarith module)\n" (solve_1());
  Printf.printf "   %d: (w/o Zarith module)\n" (solve_2())
