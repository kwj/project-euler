(* Project Euler: Problem 80 *)

(*
  This program is slow because it uses lists.

  I used a method of extracting square root which I was taught in my childhood.
 *)

(* ---------------------------------------------------------------- *)

let solve num =
  let trim_zero lst =
    let rec loop l =
      if List.hd l = 0 then
        loop (List.tl l)
      else
        List.rev l
    in
    loop (List.rev lst)
  in
  let rec cmp_nlst l1 l2 =
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      List.compare (fun e1 e2 -> e1 - e2) (List.rev l1) (List.rev l2)
    else
      if diff > 0 then
        cmp_nlst l1 (l2 @ [0])
      else
        cmp_nlst (l1 @ [0]) l2
  in
  let rec add_nlst l1 l2 =
    let rec aux l1 l2 carry acc =
      match l1, l2 with
      | hd1 :: tl1, hd2 :: tl2 ->
         aux tl1 tl2 ((hd1 + hd2 + carry) / 10) (((hd1 + hd2 + carry) mod 10) :: acc)
      | [], [] when carry > 0 ->
         List.rev (carry :: acc)
      | _ ->
         List.rev acc
    in
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      aux l1 l2 0 []
    else
      if diff > 0 then
        add_nlst l1 (l2 @ [0])
      else
        add_nlst (l1 @ [0]) l2
  in
  let rec sub_nlst l1 l2 =
    (* caution: l1 > l2 only *)
    let rec aux l1 l2 carry acc =
      match l1, l2 with
      | hd1 :: tl1, hd2 :: tl2 ->
         let tmp = hd1 - hd2 - carry in
         if tmp < 0 then
           aux tl1 tl2 (((abs tmp) + 9) / 10) ((abs ((tmp + 10) mod 10)) :: acc)
         else
           aux tl1 tl2 0 ((tmp mod 10) :: acc)
      | [], [] when carry > 0 ->
         trim_zero (List.rev (carry :: acc))
      | _ ->
         trim_zero (List.rev acc)
    in
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      if cmp_nlst l1 l2 >= 0 then
        aux l1 l2 0 []
      else
        aux l2 l1 0 []
    else
      if diff > 0 then
        sub_nlst l1 (l2 @ [0])
      else
        sub_nlst (l1 @ [0]) l2
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
  let gen_dividend num =
    let rec aux n result =
      if n = 0 then
        if List.length result mod 2 <> 0 then
          0 :: result
        else
          result
      else
        aux (n / 10) ((n mod 10) :: result)
    in
    let dividend = ref (aux num []) in
    let next l =
      if List.length !dividend = 0 then
        0 :: 0 :: l
      else
        let elm1, elm2 = List.nth !dividend 0, List.nth !dividend 1 in
        dividend := List.tl (List.tl !dividend);
        elm2 :: elm1 :: l
    in
    next
  in
  let next_step divd q =
    let rec loop x =
      let tmp = mul_nlst (add_nlst (mul_nlst q 20) [x]) x in
      if cmp_nlst tmp divd <= 0 then
        (sub_nlst divd tmp), (trim_zero (x :: q))
      else
        loop (pred x)
    in
    loop 9
  in
  let extr_sqroot g_divd divd q =
    let rec loop divd q =
      if List.length q >= 100 then
        q
      else
        let next_divd, next_q = next_step divd q in
        loop (g_divd next_divd) next_q
    in
    loop (g_divd divd) q
  in
  let rec compute n result =
    let sum100 lst =
      let rec aux lst =
        if List.length lst > 100 then
          aux (List.tl lst)
        else
          List.fold_left (+) 0 lst
      in
      aux lst
    in
    if n > num then
      result
    else
      let sqrt_n = truncate @@ sqrt @@ float n in
      if n = sqrt_n * sqrt_n then
        compute (n + 1) result
      else
        compute (n + 1) (result + (sum100 (extr_sqroot (gen_dividend n) [] [0])))
  in
  compute 1 0

let () =
  Printf.printf "Answer: %d\n" (solve 100)
