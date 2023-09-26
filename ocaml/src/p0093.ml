(* Project Euler: Problem 93 *)

open Core

let make_numbers lst =
  let res = Hash_set.create (module Int) in
  let rec aux lst =
    if List.length lst = 1
    then (
      if Z.(equal (Q.den (List.hd_exn lst)) one)
      then Hash_set.add res (Q.to_int (List.hd_exn lst)))
    else (
      let length = List.length lst in
      for i = 0 to length - 1 do
        for j = i + 1 to length - 1 do
          let next_lst = List.filteri ~f:(fun idx _ -> idx <> i && idx <> j) lst in
          let d1 = List.nth_exn lst i in
          let d2 = List.nth_exn lst j in
          aux (Q.(d1 + d2) :: next_lst);
          aux (Q.(d1 * d2) :: next_lst);
          aux (Q.(d1 - d2) :: next_lst);
          aux (Q.(d2 - d1) :: next_lst);
          if Q.(d1 <> zero) then aux (Q.(d2 / d1) :: next_lst);
          if Q.(d2 <> zero) then aux (Q.(d1 / d2) :: next_lst)
        done
      done)
  in
  aux (List.map lst ~f:(fun n -> Q.of_int n));
  res
;;

let count_consec_numbers lst =
  let number_set = make_numbers lst in
  let rec loop cnt = if Hash_set.mem number_set cnt then loop (succ cnt) else cnt - 1 in
  loop 1
;;

let compute () =
  let _, lst =
    List.range 1 9 ~stop:`inclusive
    |> Euler.Util.combination 4
    |> List.map ~f:(fun lst -> (count_consec_numbers lst, lst))
    |> List.max_elt ~compare:(fun (x, _) (y, _) -> Int.compare x y)
    |> Option.value_exn
  in
  List.rev lst |> Euler.Util.undigits
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Arithmetic Expressions" = [%test_eq: int] (compute ()) 1258
