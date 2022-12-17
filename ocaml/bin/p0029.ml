(* Project Euler: Problem 29 *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
module ZSet = Set.Make(Z)

let solve_1 () =
  let s = ref ZSet.empty in
  for a = 2 to 100 do
    for b = 2 to 100 do
      s := ZSet.add (Z.pow (Z.of_int a) b) !s
    done
  done;
  ZSet.cardinal !s

(* -- method 2 -- *)
(* using list *)
module StrSet = Set.Make(String)

let solve_2 () =
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
  let s = ref StrSet.empty in
  for a = 2 to 100 do
    let rec loop_b a_lst b =
      if b > 100 then
        ()
      else (
        let tmp = mul_nlst a_lst a in
        s := StrSet.add (List.fold_left (^) "" (List.map (string_of_int) tmp)) !s;
        loop_b tmp (succ b)
      )
    in
    loop_b (lst_of_int a) 2;
  done;
  StrSet.cardinal !s

let () =
  Printf.printf "The number of different terms is:\n";
  Printf.printf "  %d  (w/ Zarith module)\n" (solve_1());
  Printf.printf "  %d  (w/o Zarith module)\n" (solve_2())

